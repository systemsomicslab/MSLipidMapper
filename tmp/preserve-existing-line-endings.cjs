const fs = require('fs');
const cp = require('child_process');
for (const file of process.argv.slice(2)) {
  const original = cp.execFileSync('git', ['show', 'HEAD:'+file], {encoding:'utf8'});
  const current = fs.readFileSync(file,'utf8');
  const split = s => s.match(/[^\n]*\n|[^\n]+$/g) || [];
  const a = split(original), b = split(current);
  const plain = s => s.replace(/\r?\n$/, '');
  const x = a.map(plain), y = b.map(plain);
  const dp = Array.from({length:x.length+1}, () => new Uint16Array(y.length+1));
  for(let i=x.length-1;i>=0;i--) for(let j=y.length-1;j>=0;j--)
    dp[i][j] = x[i]===y[j] ? dp[i+1][j+1]+1 : Math.max(dp[i+1][j],dp[i][j+1]);
  const out=[]; let i=0,j=0;
  while(j<y.length) {
    if(i<x.length && x[i]===y[j]) {out.push(a[i]);i++;j++;}
    else if(i<x.length && dp[i+1][j]>=dp[i][j+1]) i++;
    else out.push(b[j++].replace(/\r\n$/, '\n'));
  }
  const result=out.join('');
  if(result.replace(/\r\n/g,'\n')!==current.replace(/\r\n/g,'\n')) throw Error('Content changed');
  fs.writeFileSync(file,result);
}
