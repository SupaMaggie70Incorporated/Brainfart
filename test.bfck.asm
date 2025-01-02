static counter = [4];
static abc = "Hello, world!\n";
meta registers 0;
meta stackalign 1;
meta stacksize 4;
meta staticstacksize 0;
meta entry main;

global block main() {
  push stack 3;
  goto stack[0];
  current ++ 'A';
  bfraw ".[-]";
  goto counter[0];
  bfraw "[->[.>]";
  << len abc;
  bfraw "<]";
  updateptr counter[0];
  goto stack[1];
  ++ 'B';
  bfraw ".[-]";
  pop stack 3;
  exit;
}