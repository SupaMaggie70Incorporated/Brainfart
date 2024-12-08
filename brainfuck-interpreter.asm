// The program itself will be stored in the stack. The memory will be in the heap
meta registers 4;
meta stackalign 3;
meta stacksize 256;
meta staticstacksize 0;
meta entry start;

/* Valid characters:
  + - 43
  , - 44
  - - 45
  . - 46
  < - 60
  > - 62
  [ - 91
  ] - 93
*/


block parse_bf_input() {
    bfraw ".";
    // 
}
global block start() {
    goto stack[0];
    inline parse_bf_input();
}
global block iterate() {

}