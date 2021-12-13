program mn
  character(len=20) :: ch
  character(len=20) :: dh
  ch = "kiran"
  dh = trim(ch) // '-' // "hello"
  print *, dh
end program
