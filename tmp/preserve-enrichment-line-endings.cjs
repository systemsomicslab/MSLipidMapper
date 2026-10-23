const fs = require('fs');
const cp = require('child_process');
let base = cp.execFileSync('git', ['show', 'HEAD:R/Enrich_module.R'], {encoding:'utf8'});
base = base.replace('            "Font size",', '            "Figure settings",');
base = base.replace('numericInput(ns("bar_font_size"), "Barplot font size (ggplot)", value = 12, min = 6, max = 30, step = 1)',
  'numericInput(ns("bar_font_size"), "Barplot font size (ggplot)", value = 12, min = 6, max = 30, step = 1),\r\n              textInput(ns("bar_axis_label"), "Value-axis label", value = "-log10(adjusted p)")');
base = base.replace('        adv <- .current_adv()\n        y_axis_label <- trimws(as.character(adv$lipid_y_axis_label %||% "-log10(adjusted p)"))',
  '        y_axis_label <- trimws(as.character(input$bar_axis_label %||% "-log10(adjusted p)"))');
const current = fs.readFileSync('R/Enrich_module.R','utf8');
if (base.replace(/\r\n/g,'\n') !== current.replace(/\r\n/g,'\n')) throw Error('Unexpected content differences');
fs.writeFileSync('R/Enrich_module.R', base);
