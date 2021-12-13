program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  character(len=150) :: line
  character(len=150) :: stack
  integer :: top
  integer :: i, j
  integer(kind=8) :: totalCost
  character(len=12) :: numInput1Char
  integer :: numInput1
  logical :: failed
  integer :: numIncomplete
  integer(kind=8) :: vals(94)

  vals = 0
  numIncomplete = 0


  call get_command_argument(1, inputFile)
  call get_command_argument(2, numInput1Char)
  read(numInput1Char,*) numInput1
  print *, inputFile, numInput1

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numInput1
  totalCost = 0
    failed = .false.
    top = 0
    read(fd,'(A)') line
    do i=1,len_trim(line)
       if (isOpen(line(i:i))) then
         call pushStack(line(i:i))
       else
         if (.not. popStack(line(i:i))) then
           failed = .true.
           exit
         end if
       end if
    end do

    if (.not. failed) then
            numIncomplete = numIncomplete + 1
      !print *, trim(line), len(line), len_trim(line)
      !print *, "INCOMPLETE", trim(line), ":", line(i:i), i, j
      !print *, trim(line), " : ", trim(stack(1:top))
      do while (top > 0)
        totalCost = totalCost * 5 + getCost(stack(top:top))
        !print *, totalCost
        top = top - 1
      end do
      !print *, "After ", j , " ", totalCost
      vals(numIncomplete) = totalCost
    end if
    !do i=1,len(line)
    !  heights(j,i) = IACHAR(line(i:i)) - IACHAR('0')
    !end do
  end do
  close(fd)
  print *, findMedian()
contains
  function findMedian()
    integer :: i, j
    integer(kind=8) :: findMedian
    integer (kind=8) :: tmp
    integer (kind=8) :: curVal
    !print *, vals
    do i = 1, numIncomplete-1
      curVal = vals(i+1)
      do j = i , 1, -1
        if (curVal .le. vals(j)) then
            vals(j+1) = vals(j)
        else
           vals(j+1) = curVal
           exit
        end if
      end do
      if ( j .eq. 0) then
           vals(j+1) = curVal
       end if
    !print *, vals
    end do
    if (mod(numIncomplete,2) == 0) then
      findMedian = (vals(numIncomplete/2)+ vals(numIncomplete/2-1))/2
    else
      findMedian = vals((numIncomplete+1)/2)
    end if
  end function
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

 function getCost(c)
   character :: c
   integer :: getCost
   getCost = 0
   if (c == '(') then
           getCost = 1
   else if (c == '[') then
           getCost = 2
   else if (c == '{') then
           getCost = 3
   else if (c == '<') then
           getCost = 4
   end if
 end function
end program mn
