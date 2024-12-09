// The program itself will be stored in the stack. The memory will be in the heap
// Each 3-element segment of the stack will contain the index of the operation and 2 temporary locations
meta registers 4;
meta stackalign 3;
meta stacksize 256;
meta staticstacksize 0;
meta entry start;
/*Used instructions/features
  ++/-- number
  a ++/-- number
  push/pop stack number
  if zero[] block/inline else block/inline
  a = number
  goto
  bfraw
  updateptr
  inline
  while place block {}
  exit
  Stack and heap
*/

/* Valid characters:
  + - 43
  , - 44
  - - 45
  . - 46
  ; - 59
  < - 60
  > - 62
  [ - 91
  ] - 93

Note that the semicolon signals the end of the program and the start of input.
Baked characters:
  + - 1
  - - 2
  . - 3
  , - 4
  < - 5
  > - 6
  [ - 7
  ] - 8
*/

block parse_bf_input_2() {
  -- 1;
  if zero[current] block { // <
    ++ 5;
  } else block {
    -- 2;
    if zero[current] block { // >
      ++ 6;
    } else block {
      -- 29;
      if zero[current] block { // [
        ++ 7;
      } else block {
        -- 2;
        if zero[current] block { // ]
          ++ 8;
        } else block {
          current = 0;
        }
      }
    }
  }
}
block parse_bf_input() {
  bfraw ".";
  if zero[current] block {
    r1 = 0;
  } else block {
  -- 43;
    if zero[current] block { // +
      ++ 1;
    } else block {
      -- 1;
      if zero[current] block { // ,
        ++ 4;
      } else block {
        -- 1;
        if zero[current] block { // -
          ++ 2;
        } else block {
          --1;
          if zero[current] block { // .
            ++ 3;
          } else block {
            --13;
            if zero[current] block { // ;
              r1 = 0;
            } else inline parse_bf_input_2();
          }
        }
      }
    }
  }
}
global block start() {
  r1 = 1;
  r2 = 0;
  while r1 {
    r2 ++ 1;
    push stack 1;
    goto stack[0];
    inline parse_bf_input();
  }
  // Put at the first instruction
  r2 -- 1;
  while r2 block {
    r2 -- 1;
    pop stack 1;
  }
  // Set the current location in memory, heap[0] should always be 0
  heap[2] ++ 1;
  r1 = 0;
  call iterate;
}
block goto_memory_location() {
  goto heap[2];
  bfraw "[>>]<";
}
block exit_memory() {
  bfraw "<[<<]";
  updateptr heap[0];
}
block iterate_2() {
  -- 1;
  if zero[current] block { // <
    ++ 5;
    push stack 1;
    inline goto_memory_location();
    bfraw ">+<";
    inline exit_memory();
  } else block {
    -- 1;
    if zero[current] block { // >
      ++ 6;
      push stack 1;
      inline goto_memory_location();
      bfraw "<-<";
      inline exit_memory();
    } else block {
      -- 1;
      if zero[current] block { // [
        ++ 7;
      } else block { // ]
        ++ 7;
      }
    }
  }
}
global block iterate() {
  goto stack[0];
  if zero[current] block { // End or \0
    exit;
  } else block {
    -- 1;
    if zero[current] block { // +
      ++ 1;
      push stack 1;
      inline goto_memory_location();
      ++ 1;
      inline exit_memory();
    } else block {
      -- 1;
      if zero[current] block { // -
        ++ 2;
        push stack 1;
        inline goto_memory_location();
        -- 1;
        inline exit_memory();
      } else block {
        -- 1;
        if zero[current] block { // .
          ++ 3;
          push stack 1;
          inline goto_memory_location();
          bfraw ".";
          inline exit_memory();
        } else block {
          -- 1;
          if zero[current] block { // ,
            ++ 4;
            push stack 1;
            inline goto_memory_location();
            bfraw ",";
            inline exit_memory();
          } else inline iterate_2();
        }
      }
    }
  }
}