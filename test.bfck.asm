static counter = [4];
static abc = "Hello, world!\n";
meta registers 0;
meta stackalign 1;
meta stacksize 0;
meta staticstacksize 0;
meta entry main;

global block main() {
  goto counter[0];
  bfraw "[->[.>]";
  << len abc;
  bfraw "<]";
  updateptr counter[0];
  exit;
}