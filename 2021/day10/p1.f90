program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  character(len=150) :: line
  character(len=150) :: stack
  integer :: top
  integer :: i, j
  integer :: totalCost
  character(len=12) :: numInput1Char
  integer :: numInput1

  totalCost = 0

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numInput1Char)
  read(numInput1Char,*) numInput1
  print *, inputFile, numInput1

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numInput1
    top = 0
    read(fd,'(A)') line
    do i=1,len_trim(line)
       if (isOpen(line(i:i))) then
         call pushStack(line(i:i))
       else
         if (.not. popStack(line(i:i))) then
           call addCost(line(i:i))
           print *, trim(line), len(line), len_trim(line)
           print *, "FAIL", line(i:i), j, totalCost
           exit
         end if
       end if
    end do
    !do i=1,len(line)
    !  heights(j,i) = IACHAR(line(i:i)) - IACHAR('0')
    !end do
  end do
  close(fd)
  print *, totalCost
contains
  function isOpen(c)
    character :: c
    logical :: isOpen
    if (c == '(' .or. c == '[' .or. c == '{' .or. c == '<') then
      isOpen = .true.
    else 
      isOpen = .false.
    endif
  end function

  function getMatchingClose(c)
    character :: c
    character :: getMatchingClose
    select case (c)
      case ('(')
        getMatchingClose = ')'
      case ('[')
        getMatchingClose = ']'
      case ('{')
        getMatchingClose = '}'
      case ('<')
        getMatchingClose = '>'
    end select
  end function

  function getMatchingOpen(c)
    character :: c
    character :: getMatchingOpen
    select case (c)
      case (')')
        getMatchingOpen = '('
      case (']')
        getMatchingOpen = '['
      case ('}')
        getMatchingOpen = '{'
      case ('>')
        getMatchingOpen = '<'
    end select
  end function

  subroutine pushStack(c)
    character :: c
    top = top+1
    stack(top:top) = c
  end subroutine

 function popStack(c)
    character :: c
    logical :: popStack
    if (getMatchingOpen(c) /= stack(top:top)) then
      top = 0
      popStack = .false.
    else
      top = top - 1
      popStack = .true.
    end if
 end function

 subroutine addCost(c)
   character :: c
   if (c == ')') then
     totalCost = totalCost + 3
   else if (c == ']') then
     totalCost = totalCost + 57
   else if (c == '}') then
     totalCost = totalCost + 1197
   else if (c == '>') then
     totalCost = totalCost + 25137
   end if
 end subroutine
end program mn
