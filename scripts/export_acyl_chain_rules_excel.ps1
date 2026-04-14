param(
  [string]$YamlPath = ".\R\lipid_rules.yaml",
  [string]$OutXlsx = ".\docs\acyl_chain_rule_table.xlsx",
  [string]$OutDir = ".\docs\acyl_chain_rule_tables"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Resolve-RepoPath {
  param([string]$Path)

  if ([System.IO.Path]::IsPathRooted($Path)) {
    return [System.IO.Path]::GetFullPath($Path)
  }

  return [System.IO.Path]::GetFullPath((Join-Path (Get-Location) $Path))
}

 function ConvertFrom-YamlScalar {
   param([string]$Value)
 
  $text = if ($null -eq $Value) { "" } else { $Value.Trim() }
  if ($text -eq "") { return "" }

  if (($text.StartsWith("'") -and $text.EndsWith("'")) -or ($text.StartsWith('"') -and $text.EndsWith('"'))) {
    return $text.Substring(1, $text.Length - 2)
  }

  if ($text -eq "true") { return $true }
  if ($text -eq "false") { return $false }

  if ($text -match '^-?\d+$') {
    return [int]$text
  }

  if ($text.StartsWith("[") -and $text.EndsWith("]")) {
    $inner = $text.Substring(1, $text.Length - 2).Trim()
    if ($inner -eq "") { return @() }
    return @(
      foreach ($part in ($inner -split '\s*,\s*')) {
        ConvertFrom-YamlScalar $part
      }
    )
  }

  return $text
}

function Read-LipidRulesYaml {
  param([string]$Path)

  $defaults = @{}
  $patterns = New-Object System.Collections.Generic.List[object]
  $currentSection = ""
  $currentPattern = $null

  foreach ($rawLine in [System.IO.File]::ReadAllLines($Path)) {
    $line = $rawLine.TrimEnd()
    $trimmed = $line.Trim()

    if ($trimmed -eq "" -or $trimmed.StartsWith("#")) {
      continue
    }

    if ($trimmed -eq "defaults:") {
      $currentSection = "defaults"
      $currentPattern = $null
      continue
    }

    if ($trimmed -eq "patterns:") {
      $currentSection = "patterns"
      $currentPattern = $null
      continue
    }

    if ($currentSection -eq "defaults" -and $line -match '^\s{2}([^:]+):\s*(.*)$') {
      $defaults[$matches[1].Trim()] = ConvertFrom-YamlScalar $matches[2]
      continue
    }

    if ($currentSection -eq "patterns" -and $line -match '^\s{2}-\s*([^:]+):\s*(.*)$') {
      if ($null -ne $currentPattern) {
        $patterns.Add([pscustomobject]$currentPattern)
      }
      $currentPattern = [ordered]@{}
      $currentPattern[$matches[1].Trim()] = ConvertFrom-YamlScalar $matches[2]
      continue
    }

    if ($currentSection -eq "patterns" -and $line -match '^\s{4}([^:]+):\s*(.*)$') {
      if ($null -eq $currentPattern) {
        throw "Invalid YAML structure near line: $rawLine"
      }
      $currentPattern[$matches[1].Trim()] = ConvertFrom-YamlScalar $matches[2]
      continue
    }
  }

  if ($null -ne $currentPattern) {
    $patterns.Add([pscustomobject]$currentPattern)
  }

  $result = New-Object PSObject
  $result | Add-Member -MemberType NoteProperty -Name defaults -Value $defaults
  $result | Add-Member -MemberType NoteProperty -Name patterns -Value ([object[]]$patterns.ToArray())
  return $result
}

function Get-PropValue {
  param(
    [object]$Object,
    [string]$Name,
    $Default = $null
  )

  if ($null -eq $Object) {
    return $Default
  }

  $prop = $Object.PSObject.Properties[$Name]
  if ($null -eq $prop) {
    return $Default
  }

  return $prop.Value
}

 function Get-RuleScore {
   param([pscustomobject]$Rule)
 
  $priority = 0
  $priorityValue = Get-PropValue -Object $Rule -Name "priority" -Default $null
  if ($null -ne $priorityValue) {
    $priority = [int]$priorityValue
  }

  $classRegexValue = Get-PropValue -Object $Rule -Name "class_regex" -Default ""
  $classRegex = if ($null -eq $classRegexValue) { "" } else { "$classRegexValue" }
  $regexLength = $classRegex.Length
  $bonus = 0
  if ((Get-PropValue -Object $Rule -Name "no_chain" -Default $false) -eq $true) { $bonus += 80 }
  if ((Get-PropValue -Object $Rule -Name "ignore_sum_only" -Default $false) -eq $true) { $bonus += 20 }
  if ((Get-PropValue -Object $Rule -Name "exclude_first_chain" -Default $false) -eq $true) { $bonus += 25 }
  if ((Get-PropValue -Object $Rule -Name "detect_fa_suffix" -Default $false) -eq $true) { $bonus += 10 }
  if ((Get-PropValue -Object $Rule -Name "detect_fa_tail" -Default $false) -eq $true) { $bonus += 12 }
  if ((Get-PropValue -Object $Rule -Name "drop_zero_chains" -Default $false) -eq $true) { $bonus += 5 }
  if ((Get-PropValue -Object $Rule -Name "prefer_molecular_notation" -Default $false) -eq $true) { $bonus += 2 }

  return ($priority * 1000) + $regexLength + $bonus
}

 function Expand-ClassRegex {
   param([string]$Regex)
 
  $text = if ($null -eq $Regex) { "" } else { $Regex.Trim() }
  if ($text -match '^\^\(([A-Za-z0-9_\-\+\|;]+)\)\$$') {
    return [pscustomobject]@{
      scope_type = "literal_set"
      classes = @($matches[1] -split '\|')
      scope_label = ($matches[1] -split '\|') -join ", "
    }
  }

  if ($text -match '^\^([A-Za-z0-9_\-+;]+)\$$') {
    return [pscustomobject]@{
      scope_type = "literal"
      classes = @($matches[1])
      scope_label = $matches[1]
    }
  }

  if ($text -eq '^Cer_.+$') {
    return [pscustomobject]@{
      scope_type = "pattern_family"
      classes = @()
      scope_label = "Cer_*"
    }
  }

  [pscustomobject]@{
    scope_type = "pattern_family"
    classes = @()
    scope_label = $text
  }
}

function Get-RuleBehaviorSummary {
  param([pscustomobject]$Rule)

  $parts = New-Object System.Collections.Generic.List[string]

  if ((Get-PropValue -Object $Rule -Name "no_chain" -Default $false) -eq $true) {
    $parts.Add("No acyl-chain metadata is extracted.")
  } else {
    $splitValue = Get-PropValue -Object $Rule -Name "split" -Default $null
    $split = if ($null -ne $splitValue -and "$splitValue".Trim() -ne "") { "$splitValue" } else { "auto" }
    if ($split -eq "auto") {
      $parts.Add("Split order: '/' -> '_' -> whitespace -> unsplit.")
    } else {
      $splitModeValue = Get-PropValue -Object $Rule -Name "split_mode" -Default $null
      $splitMode = if ($null -ne $splitModeValue -and "$splitModeValue".Trim() -ne "") { " ($splitModeValue)" } else { "" }
      $parts.Add("Split delimiter: $split$splitMode.")
    }

    if ((Get-PropValue -Object $Rule -Name "ignore_sum_only" -Default $false) -eq $true) {
      $parts.Add("Skip sum-only notation when molecular/sn evidence is absent.")
    }
    if ((Get-PropValue -Object $Rule -Name "exclude_first_chain" -Default $false) -eq $true) {
      $parts.Add("Drop the first chain when two or more chain tokens are present.")
    }
    if ((Get-PropValue -Object $Rule -Name "detect_fa_suffix" -Default $false) -eq $true) {
      $parts.Add("Extract FA-style chains from final parentheses.")
    }
    if ((Get-PropValue -Object $Rule -Name "detect_fa_tail" -Default $false) -eq $true) {
      $parts.Add("Extract trailing 'FA xx:y' notation outside parentheses.")
    }
    if ((Get-PropValue -Object $Rule -Name "drop_zero_chains" -Default $false) -eq $true) {
      $parts.Add("Remove zero chains such as 0:0.")
    }
    if ((Get-PropValue -Object $Rule -Name "prefer_molecular_notation" -Default $false) -eq $true) {
      $parts.Add("Prefer alternatives containing '/' or '_' when 'A|B' notation exists.")
    }
  }

  if ($parts.Count -eq 0) {
    $parts.Add("Fallback parsing behavior.")
  }

  return ($parts -join " ")
}

function Get-LegendRows {
  return @(
    [pscustomobject]@{ item = "no_chain"; meaning = "If TRUE, no acyl-chain metadata is returned." },
    [pscustomobject]@{ item = "ignore_sum_only"; meaning = "Ignore sum-only notation when there is no molecular or sn-position evidence." },
    [pscustomobject]@{ item = "split"; meaning = "How the main chain section is split. 'auto' means '/', then '_', then whitespace." },
    [pscustomobject]@{ item = "split_mode"; meaning = "Whether 'split' is interpreted as literal/fixed text or as a regex." },
    [pscustomobject]@{ item = "exclude_first_chain"; meaning = "When two or more chains are present, drop the first chain token." },
    [pscustomobject]@{ item = "detect_fa_suffix"; meaning = "Extract chains from FA suffixes in parentheses such as '(FA 22:6)'." },
    [pscustomobject]@{ item = "detect_fa_tail"; meaning = "Extract trailing 'FA xx:y' notation outside parentheses." },
    [pscustomobject]@{ item = "drop_zero_chains"; meaning = "Remove placeholder chains such as 0:0." },
    [pscustomobject]@{ item = "priority"; meaning = "Rule precedence when multiple regexes match. Larger numbers win." },
    [pscustomobject]@{ item = "score"; meaning = "Internal score equivalent to the R .score_rule implementation." },
    [pscustomobject]@{ item = "Effective Classes"; meaning = "One-row-per-class table showing the final effective rule after precedence resolution." },
    [pscustomobject]@{ item = "Regex Patterns"; meaning = "Raw YAML rules table, including overlaps and overridden patterns." }
  )
}

function Get-ClassCategory {
  param(
    [string]$LipidClass,
    [string]$RuleId
  )

  if ($RuleId -in @("bile_acid", "sterol_free", "steryl_ester", "cholesteryl_ester", "vae", "asg_drop_sterol")) {
    return "Sterol-related lipid"
  }
  if ($RuleId -in @("ceramide_force_drop_base", "cer_subclass_force_drop_base", "sphingolipid_generic", "gm3_mixed_notation", "spb_no_chain", "ngcgm3")) {
    return "Sphingolipid"
  }
  if ($RuleId -eq "triglyceride") {
    return "Neutral glycerolipid"
  }
  if ($RuleId -eq "glycerolipid") {
    return "Glycerolipid"
  }
  if ($RuleId -eq "phospholipid") {
    return "Glycerophospholipid"
  }
  if ($RuleId -in @("fatty_acid", "oxidized_fatty_acid", "acylcarnitine")) {
    return "Single-chain lipid"
  }
  if ($RuleId -eq "lnape") {
    return "N-acyl lysophospholipid"
  }

  if ($LipidClass -like "Cer_*") {
    return "Ceramide subclass"
  }

  return "Other lipid class"
}

function Get-ReportedChainsText {
  param([pscustomobject]$Row)

  if ($Row.no_chain -eq $true) {
    return "No acyl-chain metadata are reported for this class."
  }

  switch ($Row.selected_rule_id) {
    "triglyceride" { return "All detectable acyl chains are reported as separate chain metadata entries." }
    "steryl_ester" { return "The esterified fatty acyl chain is reported." }
    "cholesteryl_ester" { return "The single esterified fatty acyl chain is reported." }
    "asg_drop_sterol" { return "The fatty acyl chain is reported after excluding the sterol-derived moiety." }
    "ceramide_force_drop_base" { return "Only the N-acyl chain is reported; the long-chain base is removed before reporting." }
    "cer_subclass_force_drop_base" { return "Only the N-acyl chain is reported; the ceramide long-chain base is removed before reporting." }
    "gm3_mixed_notation" { return "Only the fatty acyl chain is reported; the long-chain base is removed before reporting." }
    "sphingolipid_generic" { return "Chain parsing follows sphingolipid notation, but the long-chain base is retained unless a higher-priority override applies." }
    "lnape" { return "The chain parsed from the main notation is reported, and FA suffixes in parentheses are additionally captured." }
    "fatty_acid" { return "The single fatty acyl chain is reported directly." }
    "oxidized_fatty_acid" { return "The oxidized fatty acyl chain is reported directly, including oxygen annotations." }
    "acylcarnitine" { return "The acyl chain linked to carnitine is reported directly." }
    "phospholipid" { return "All chain tokens present in molecular-species notation are reported." }
    "glycerolipid" { return "All chain tokens present in the lipid name are reported." }
    "vae" { return "The single acyl-like chain token is reported directly." }
    default { return "Chain tokens matching the implemented notation are reported." }
  }
}

function Get-ExcludedMoietyText {
  param([pscustomobject]$Row)

  if ($Row.no_chain -eq $true) {
    return "Not applicable."
  }
  if ($Row.exclude_first_chain -eq $true) {
    switch ($Row.selected_rule_id) {
      "asg_drop_sterol" { return "The first token is removed and interpreted as the sterol-derived moiety." }
      "steryl_ester" { return "The first token is removed and interpreted as the sterol backbone." }
      "ceramide_force_drop_base" { return "The first token is removed and interpreted as the sphingoid long-chain base." }
      "cer_subclass_force_drop_base" { return "The first token is removed and interpreted as the sphingoid long-chain base." }
      "gm3_mixed_notation" { return "The first token is removed and interpreted as the sphingoid long-chain base." }
      default { return "The first chain token is excluded when two or more chain tokens are present." }
    }
  }

  return "No class-specific backbone removal is applied."
}

function Get-NotationText {
  param([pscustomobject]$Row)

  if ($Row.no_chain -eq $true) {
    return "No chain notation is parsed."
  }
  if ($Row.split -eq "auto") {
    return "Molecular-species notation is parsed using '/' first, then '_', then whitespace if needed."
  }
  if ($Row.split -eq "/") {
    return "Slash-delimited notation is parsed explicitly."
  }
  if ([string]::IsNullOrWhiteSpace($Row.split)) {
    return "No explicit split delimiter is defined."
  }

  return "Primary delimiter: $($Row.split)."
}

function Get-SumOnlyText {
  param([pscustomobject]$Row)

  if ($Row.no_chain -eq $true) {
    return "Not applicable."
  }
  if ($Row.ignore_sum_only -eq $true) {
    return "Sum-composition-only names are ignored unless molecular-species evidence is present."
  }

  return "Sum-composition-only names are not explicitly excluded by the class rule."
}

function Get-SpecialHandlingText {
  param([pscustomobject]$Row)

  if ($Row.no_chain -eq $true) {
    return "None."
  }

  $parts = New-Object System.Collections.Generic.List[string]
  if ($Row.detect_fa_suffix -eq $true) {
    $parts.Add("FA suffixes in parentheses are captured")
  }
  if ($Row.detect_fa_tail -eq $true) {
    $parts.Add("trailing 'FA xx:y' text is captured")
  }
  if ($Row.drop_zero_chains -eq $true) {
    $parts.Add("0:0 placeholder chains are removed")
  }

  $parts.Add("O-/P- prefixes are stripped from reported chain tokens")
  $parts.Add("';nO' oxygen notation is normalized to ';On'")
  $parts.Add("'18:1(2OH)' is normalized to '18:1;(2OH)'")

  return ($parts -join "; ") + "."
}

function Get-RepresentativeInterpretationText {
  param([pscustomobject]$Row)

  switch ($Row.selected_rule_id) {
    "phospholipid" { return "Example: PC 16:0/18:1 -> reported chains 16:0 and 18:1." }
    "glycerolipid" { return "Example: DG 16:0_18:1 -> reported chains 16:0 and 18:1." }
    "triglyceride" { return "Example: TG 16:0/18:1/18:2 -> reported chains 16:0, 18:1, and 18:2." }
    "ceramide_force_drop_base" { return "Example: Cer 18:1;O2/24:1 -> reported chain 24:1 only." }
    "cer_subclass_force_drop_base" { return "Example: Cer_NS 18:1;O2/24:0 -> reported chain 24:0 only." }
    "gm3_mixed_notation" { return "Example: GM3 18:1;O2/24:1 -> reported chain 24:1 only." }
    "steryl_ester" { return "Example: SE 27:1/18:2 -> reported chain 18:2 only." }
    "cholesteryl_ester" { return "Example: CE 20:4 -> reported chain 20:4." }
    "asg_drop_sterol" { return "Example: ASG 27:1;FA 18:2 -> reported chain 18:2 only." }
    "fatty_acid" { return "Example: FA 20:4 -> reported chain 20:4." }
    "oxidized_fatty_acid" { return "Example: OxFA 20:4;O2 -> reported chain 20:4;O2." }
    "acylcarnitine" { return "Example: CAR 16:0 -> reported chain 16:0." }
    "lnape" { return "Example: LNAPE 18:1 (FA 20:4) -> reported chains 18:1 and 20:4." }
    "bile_acid" { return "Example: BA -> no acyl-chain metadata reported." }
    "sterol_free" { return "Example: ST -> no acyl-chain metadata reported." }
    "spb_no_chain" { return "Example: SPB -> no acyl-chain metadata reported." }
    default {
      if ($Row.lipid_class -like "Cer_*") {
        return "Example: Cer_* 18:1;O2/24:0 -> reported chain 24:0 only."
      }
      return "Interpretation follows the class-specific rule listed in this table."
    }
  }
}

function Get-ParserNotesRows {
  return @(
    [pscustomobject]@{ section = "Rule precedence"; note = "If multiple YAML patterns match the same lipid class, the highest internal score is selected, following the .score_rule logic implemented in the parser." },
    [pscustomobject]@{ section = "Auto split"; note = "For classes with split='auto', chain parsing tries '/' first, then '_', then whitespace, and otherwise leaves the remaining string unsplit." },
    [pscustomobject]@{ section = "Sum composition"; note = "When ignore_sum_only=TRUE, names with only one chain token and without '/', '_', FA suffixes, or tail FA annotations are treated as sum-composition-only and no chain metadata are returned." },
    [pscustomobject]@{ section = "Alternative notation"; note = "If a lipid name contains 'A|B', the parser keeps the alternative containing the largest number of chain-like tokens." },
    [pscustomobject]@{ section = "Chain normalization"; note = "Reported chain tokens are normalized by removing FA, O-, and P- prefixes; converting ';2O' to ';O2'; converting '18:1(2OH)' to '18:1;(2OH)'; and discarding 0:0 placeholder chains." },
    [pscustomobject]@{ section = "Backbone removal"; note = "For classes with exclude_first_chain=TRUE, the first chain-like token is removed only when two or more chain tokens are present." },
    [pscustomobject]@{ section = "Suffix extraction"; note = "detect_fa_suffix captures chains encoded in parentheses, while detect_fa_tail captures trailing 'FA xx:y' annotations outside parentheses." },
    [pscustomobject]@{ section = "Human-readable table"; note = "The main supplement table below is a prose rendering of the implemented YAML rules in R/lipid_rules.yaml and the extraction logic in R/lipid_chain_parser_se.R." }
  )
}

function Convert-ObjectsToRows {
  param([object[]]$Objects)

  if ($null -eq $Objects -or $Objects.Count -eq 0) {
    return ,@(@("empty"))
  }

  $headers = @($Objects[0].PSObject.Properties.Name)
  $rows = New-Object System.Collections.Generic.List[object[]]
  $rows.Add($headers)

  foreach ($obj in $Objects) {
    $row = foreach ($header in $headers) {
      $value = $obj.$header
      if ($null -eq $value) {
        ""
      } elseif ($value -is [System.Array]) {
        ($value | ForEach-Object { "$_" }) -join ", "
      } else {
        "$value"
      }
    }
    $rows.Add(@($row))
  }

  return @($rows)
}

function Export-CsvTable {
  param(
    [object[]]$Objects,
    [string]$Path
  )

  $parent = Split-Path -Parent $Path
  if (-not [string]::IsNullOrWhiteSpace($parent) -and -not (Test-Path $parent)) {
    [System.IO.Directory]::CreateDirectory($parent) | Out-Null
  }

  $Objects | Export-Csv -NoTypeInformation -Encoding UTF8 -Path $Path
}

function Get-ExcelColumnName {
  param([int]$ColumnNumber)

  $name = ""
  $n = $ColumnNumber
  while ($n -gt 0) {
    $remainder = ($n - 1) % 26
    $name = [char](65 + $remainder) + $name
    $n = [math]::Floor(($n - 1) / 26)
  }
  return $name
}

 function Escape-XmlText {
   param([string]$Text)
 
  $safeText = if ($null -eq $Text) { "" } else { $Text }
  return [System.Security.SecurityElement]::Escape($safeText)
 }

function New-WorksheetXml {
  param(
    [string]$SheetName,
    [object[]]$Rows
  )

  $rowCount = $Rows.Count
  $colCount = 0
  foreach ($row in $Rows) {
    if ($row.Count -gt $colCount) {
      $colCount = $row.Count
    }
  }

  $dimension = if ($rowCount -gt 0 -and $colCount -gt 0) {
    "A1:{0}{1}" -f (Get-ExcelColumnName $colCount), $rowCount
  } else {
    "A1"
  }

  $sheetData = New-Object System.Text.StringBuilder
  for ($r = 0; $r -lt $rowCount; $r++) {
    [void]$sheetData.Append("<row r=""$($r + 1)"">")
    $row = $Rows[$r]
    for ($c = 0; $c -lt $row.Count; $c++) {
      $ref = "{0}{1}" -f (Get-ExcelColumnName ($c + 1)), ($r + 1)
      $styleAttr = if ($r -eq 0) { ' s="1"' } else { "" }
      $value = Escape-XmlText "$($row[$c])"
      [void]$sheetData.Append("<c r=""$ref"" t=""inlineStr""$styleAttr><is><t xml:space=""preserve"">$value</t></is></c>")
    }
    [void]$sheetData.Append("</row>")
  }

  return @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <dimension ref="$dimension"/>
  <sheetViews><sheetView workbookViewId="0"/></sheetViews>
  <sheetFormatPr defaultRowHeight="15"/>
  <sheetData>$sheetData</sheetData>
</worksheet>
"@
}

function Write-XlsxFile {
  param(
    [hashtable[]]$Sheets,
    [string]$Path
  )

  $parent = Split-Path -Parent $Path
  if (-not [string]::IsNullOrWhiteSpace($parent) -and -not (Test-Path $parent)) {
    [System.IO.Directory]::CreateDirectory($parent) | Out-Null
  }

  Add-Type -AssemblyName System.IO.Compression
  Add-Type -AssemblyName System.IO.Compression.FileSystem

  if (Test-Path $Path) {
    Remove-Item -LiteralPath $Path -Force
  }

  $archive = [System.IO.Compression.ZipFile]::Open($Path, [System.IO.Compression.ZipArchiveMode]::Create)
  try {
    $sheetEntries = @()
    for ($i = 0; $i -lt $Sheets.Count; $i++) {
      $sheetIndex = $i + 1
      $sheetRows = $Sheets[$i].rows
      $sheetName = $Sheets[$i].name
      $sheetXml = New-WorksheetXml -SheetName $sheetName -Rows $sheetRows
      $sheetEntries += [pscustomobject]@{
        index = $sheetIndex
        name = $sheetName
      }

      $entry = $archive.CreateEntry("xl/worksheets/sheet$sheetIndex.xml")
      $writer = New-Object System.IO.StreamWriter($entry.Open(), [System.Text.UTF8Encoding]::new($false))
      try {
        $writer.Write($sheetXml)
      } finally {
        $writer.Dispose()
      }
    }

    $contentTypes = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
  <Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>
  <Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
  <Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
$(
  ($sheetEntries | ForEach-Object {
    "  <Override PartName=""/xl/worksheets/sheet$($_.index).xml"" ContentType=""application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml""/>"
  }) -join "`n"
)
</Types>
"@

    $rootRels = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
  <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>
  <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>
</Relationships>
"@

    $workbookXml = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <bookViews><workbookView xWindow="0" yWindow="0" windowWidth="24000" windowHeight="12000"/></bookViews>
  <sheets>
$(
  ($sheetEntries | ForEach-Object {
    "    <sheet name=""$(Escape-XmlText $_.name)"" sheetId=""$($_.index)"" r:id=""rId$($_.index)""/>"
  }) -join "`n"
)
  </sheets>
</workbook>
"@

    $workbookRels = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
$(
  ($sheetEntries | ForEach-Object {
    "  <Relationship Id=""rId$($_.index)"" Type=""http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"" Target=""worksheets/sheet$($_.index).xml""/>"
  }) -join "`n"
)
  <Relationship Id="rId$($sheetEntries.Count + 1)" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>
</Relationships>
"@

    $stylesXml = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <fonts count="2">
    <font><sz val="11"/><name val="Calibri"/></font>
    <font><b/><sz val="11"/><name val="Calibri"/></font>
  </fonts>
  <fills count="2">
    <fill><patternFill patternType="none"/></fill>
    <fill><patternFill patternType="gray125"/></fill>
  </fills>
  <borders count="1">
    <border><left/><right/><top/><bottom/><diagonal/></border>
  </borders>
  <cellStyleXfs count="1">
    <xf numFmtId="0" fontId="0" fillId="0" borderId="0"/>
  </cellStyleXfs>
  <cellXfs count="2">
    <xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>
    <xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>
  </cellXfs>
  <cellStyles count="1">
    <cellStyle name="Normal" xfId="0" builtinId="0"/>
  </cellStyles>
</styleSheet>
"@

    $created = (Get-Date).ToUniversalTime().ToString("s") + "Z"
    $coreXml = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <dc:creator>Codex</dc:creator>
  <cp:lastModifiedBy>Codex</cp:lastModifiedBy>
  <dcterms:created xsi:type="dcterms:W3CDTF">$created</dcterms:created>
  <dcterms:modified xsi:type="dcterms:W3CDTF">$created</dcterms:modified>
  <dc:title>MSLipidMapper acyl chain rules</dc:title>
</cp:coreProperties>
"@

    $titles = ($sheetEntries | ForEach-Object { "<vt:lpstr>$(Escape-XmlText $_.name)</vt:lpstr>" }) -join ""
    $appXml = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
  <Application>Microsoft Excel</Application>
  <HeadingPairs>
    <vt:vector size="2" baseType="variant">
      <vt:variant><vt:lpstr>Worksheets</vt:lpstr></vt:variant>
      <vt:variant><vt:i4>$($sheetEntries.Count)</vt:i4></vt:variant>
    </vt:vector>
  </HeadingPairs>
  <TitlesOfParts>
    <vt:vector size="$($sheetEntries.Count)" baseType="lpstr">$titles</vt:vector>
  </TitlesOfParts>
</Properties>
"@

    $files = @(
      @{ path = "[Content_Types].xml"; text = $contentTypes },
      @{ path = "_rels/.rels"; text = $rootRels },
      @{ path = "xl/workbook.xml"; text = $workbookXml },
      @{ path = "xl/_rels/workbook.xml.rels"; text = $workbookRels },
      @{ path = "xl/styles.xml"; text = $stylesXml },
      @{ path = "docProps/core.xml"; text = $coreXml },
      @{ path = "docProps/app.xml"; text = $appXml }
    )

    foreach ($file in $files) {
      $entry = $archive.CreateEntry($file.path)
      $writer = New-Object System.IO.StreamWriter($entry.Open(), [System.Text.UTF8Encoding]::new($false))
      try {
        $writer.Write($file.text)
      } finally {
        $writer.Dispose()
      }
    }
  } finally {
    $archive.Dispose()
  }
}

$yamlFullPath = Resolve-RepoPath $YamlPath
$xlsxFullPath = Resolve-RepoPath $OutXlsx
$outDirFullPath = Resolve-RepoPath $OutDir

if (-not (Test-Path $yamlFullPath)) {
  throw "YAML file not found: $yamlFullPath"
}

if (-not (Test-Path $outDirFullPath)) {
  [System.IO.Directory]::CreateDirectory($outDirFullPath) | Out-Null
}

$rules = Read-LipidRulesYaml -Path $yamlFullPath
$patterns = @($rules.patterns)

$rawRows = foreach ($rule in $patterns) {
  $ruleId = Get-PropValue -Object $rule -Name "id" -Default ""
  $classRegex = Get-PropValue -Object $rule -Name "class_regex" -Default ""
  $priority = Get-PropValue -Object $rule -Name "priority" -Default 0
  $split = Get-PropValue -Object $rule -Name "split" -Default ""
  $splitMode = Get-PropValue -Object $rule -Name "split_mode" -Default ""
  $expansion = Expand-ClassRegex "$classRegex"
  [pscustomobject]@{
    rule_id = "$ruleId"
    class_regex = "$classRegex"
    scope_type = $expansion.scope_type
    scope_label = $expansion.scope_label
    priority = [int]$priority
    score = Get-RuleScore $rule
    no_chain = [bool]((Get-PropValue -Object $rule -Name "no_chain" -Default $false) -eq $true)
    ignore_sum_only = [bool]((Get-PropValue -Object $rule -Name "ignore_sum_only" -Default $false) -eq $true)
    split = "$split"
    split_mode = "$splitMode"
    exclude_first_chain = [bool]((Get-PropValue -Object $rule -Name "exclude_first_chain" -Default $false) -eq $true)
    detect_fa_suffix = [bool]((Get-PropValue -Object $rule -Name "detect_fa_suffix" -Default $false) -eq $true)
    detect_fa_tail = [bool]((Get-PropValue -Object $rule -Name "detect_fa_tail" -Default $false) -eq $true)
    drop_zero_chains = [bool]((Get-PropValue -Object $rule -Name "drop_zero_chains" -Default $false) -eq $true)
    prefer_molecular_notation = [bool]((Get-PropValue -Object $rule -Name "prefer_molecular_notation" -Default $false) -eq $true)
    behavior_summary = Get-RuleBehaviorSummary $rule
  }
}

$literalClasses = New-Object System.Collections.Generic.HashSet[string]
$patternFamilyRows = New-Object System.Collections.Generic.List[object]

foreach ($rule in $patterns) {
  $ruleId = Get-PropValue -Object $rule -Name "id" -Default ""
  $classRegex = Get-PropValue -Object $rule -Name "class_regex" -Default ""
  $priority = Get-PropValue -Object $rule -Name "priority" -Default 0
  $split = Get-PropValue -Object $rule -Name "split" -Default ""
  $splitMode = Get-PropValue -Object $rule -Name "split_mode" -Default ""
  $expansion = Expand-ClassRegex "$classRegex"
  foreach ($className in $expansion.classes) {
    [void]$literalClasses.Add($className)
  }

  if ($expansion.scope_type -eq "pattern_family") {
    $patternFamilyRows.Add([pscustomobject]@{
      lipid_class = $expansion.scope_label
      scope_type = "pattern_family"
      matched_rule_ids = "$ruleId"
      selected_rule_id = "$ruleId"
      priority = [int]$priority
      score = Get-RuleScore $rule
      no_chain = [bool]((Get-PropValue -Object $rule -Name "no_chain" -Default $false) -eq $true)
      ignore_sum_only = [bool]((Get-PropValue -Object $rule -Name "ignore_sum_only" -Default $false) -eq $true)
      split = "$split"
      split_mode = "$splitMode"
      exclude_first_chain = [bool]((Get-PropValue -Object $rule -Name "exclude_first_chain" -Default $false) -eq $true)
      detect_fa_suffix = [bool]((Get-PropValue -Object $rule -Name "detect_fa_suffix" -Default $false) -eq $true)
      detect_fa_tail = [bool]((Get-PropValue -Object $rule -Name "detect_fa_tail" -Default $false) -eq $true)
      drop_zero_chains = [bool]((Get-PropValue -Object $rule -Name "drop_zero_chains" -Default $false) -eq $true)
      prefer_molecular_notation = [bool]((Get-PropValue -Object $rule -Name "prefer_molecular_notation" -Default $false) -eq $true)
      notes = "Regex family entry; applies to classes matching $classRegex."
      behavior_summary = Get-RuleBehaviorSummary $rule
    })
  }
}

$effectiveRows = New-Object System.Collections.Generic.List[object]
foreach ($lipidClass in ($literalClasses | Sort-Object)) {
  $matched = @(
    foreach ($rule in $patterns) {
      $ruleRegex = Get-PropValue -Object $rule -Name "class_regex" -Default ""
      if ($lipidClass -match "$ruleRegex") {
        $rule
      }
    }
  )

  if ($matched.Count -eq 0) {
    continue
  }

  $selected = $matched | Sort-Object @{ Expression = { Get-RuleScore $_ } ; Descending = $true } | Select-Object -First 1
  $selectedId = Get-PropValue -Object $selected -Name "id" -Default ""
  $selectedPriority = Get-PropValue -Object $selected -Name "priority" -Default 0
  $selectedSplit = Get-PropValue -Object $selected -Name "split" -Default ""
  $selectedSplitMode = Get-PropValue -Object $selected -Name "split_mode" -Default ""
  $effectiveRows.Add([pscustomobject]@{
    lipid_class = $lipidClass
    scope_type = "literal"
    matched_rule_ids = ($matched | ForEach-Object { "$($_.id)" }) -join ", "
    selected_rule_id = "$selectedId"
    priority = [int]$selectedPriority
    score = Get-RuleScore $selected
    no_chain = [bool]((Get-PropValue -Object $selected -Name "no_chain" -Default $false) -eq $true)
    ignore_sum_only = [bool]((Get-PropValue -Object $selected -Name "ignore_sum_only" -Default $false) -eq $true)
    split = "$selectedSplit"
    split_mode = "$selectedSplitMode"
    exclude_first_chain = [bool]((Get-PropValue -Object $selected -Name "exclude_first_chain" -Default $false) -eq $true)
    detect_fa_suffix = [bool]((Get-PropValue -Object $selected -Name "detect_fa_suffix" -Default $false) -eq $true)
    detect_fa_tail = [bool]((Get-PropValue -Object $selected -Name "detect_fa_tail" -Default $false) -eq $true)
    drop_zero_chains = [bool]((Get-PropValue -Object $selected -Name "drop_zero_chains" -Default $false) -eq $true)
    prefer_molecular_notation = [bool]((Get-PropValue -Object $selected -Name "prefer_molecular_notation" -Default $false) -eq $true)
    notes = if ($matched.Count -gt 1) { "Multiple regexes matched; the highest score rule is selected." } else { "" }
    behavior_summary = Get-RuleBehaviorSummary $selected
  })
}

foreach ($row in $patternFamilyRows) {
  $effectiveRows.Add($row)
}

$effectiveRows = @($effectiveRows | Sort-Object scope_type, lipid_class)
$rawRows = @($rawRows | Sort-Object -Property @{ Expression = "priority"; Descending = $true }, "rule_id")
$legendRows = Get-LegendRows

$supplementRows = @(
  $effectiveRows |
    Where-Object { $_.scope_type -eq "literal" } |
    ForEach-Object {
      [pscustomobject]@{
        lipid_class = $_.lipid_class
        structural_category = Get-ClassCategory -LipidClass $_.lipid_class -RuleId $_.selected_rule_id
        reported_acyl_chain_metadata = Get-ReportedChainsText $_
        excluded_backbone_or_nonacyl_moiety = Get-ExcludedMoietyText $_
        accepted_notation = Get-NotationText $_
        handling_of_sum_composition_only_names = Get-SumOnlyText $_
        special_handling_and_normalization = Get-SpecialHandlingText $_
        representative_interpretation = Get-RepresentativeInterpretationText $_
        implemented_rule_id = $_.selected_rule_id
      }
    }
)

$familyRows = @(
  $effectiveRows |
    Where-Object { $_.scope_type -eq "pattern_family" } |
    ForEach-Object {
      [pscustomobject]@{
        lipid_class_family = $_.lipid_class
        structural_category = Get-ClassCategory -LipidClass $_.lipid_class -RuleId $_.selected_rule_id
        applied_rule = $_.selected_rule_id
        human_readable_rule = Get-ReportedChainsText $_
        excluded_backbone_or_nonacyl_moiety = Get-ExcludedMoietyText $_
        accepted_notation = Get-NotationText $_
        special_handling_and_normalization = Get-SpecialHandlingText $_
      }
    }
)

$parserNotesRows = Get-ParserNotesRows

Export-CsvTable -Objects $effectiveRows -Path (Join-Path $outDirFullPath "effective_lipid_classes.csv")
Export-CsvTable -Objects $rawRows -Path (Join-Path $outDirFullPath "regex_patterns.csv")
Export-CsvTable -Objects $legendRows -Path (Join-Path $outDirFullPath "legend.csv")
Export-CsvTable -Objects $supplementRows -Path (Join-Path $outDirFullPath "supplement_human_readable_table.csv")
Export-CsvTable -Objects $familyRows -Path (Join-Path $outDirFullPath "supplement_family_rules.csv")
Export-CsvTable -Objects $parserNotesRows -Path (Join-Path $outDirFullPath "supplement_parser_notes.csv")

$sheets = @(
  @{ name = "Effective Classes"; rows = Convert-ObjectsToRows $effectiveRows },
  @{ name = "Regex Patterns"; rows = Convert-ObjectsToRows $rawRows },
  @{ name = "Legend"; rows = Convert-ObjectsToRows $legendRows }
)

Write-XlsxFile -Sheets $sheets -Path $xlsxFullPath

$supplementXlsxPath = [System.IO.Path]::Combine([System.IO.Path]::GetDirectoryName($xlsxFullPath), "acyl_chain_rule_table_supplement.xlsx")
$supplementSheets = @(
  @{ name = "Class Summary"; rows = Convert-ObjectsToRows $supplementRows },
  @{ name = "Family Rules"; rows = Convert-ObjectsToRows $familyRows },
  @{ name = "Parser Notes"; rows = Convert-ObjectsToRows $parserNotesRows }
)

Write-XlsxFile -Sheets $supplementSheets -Path $supplementXlsxPath

Write-Output "Created: $xlsxFullPath"
Write-Output "Created: $supplementXlsxPath"
Write-Output "CSV directory: $outDirFullPath"
