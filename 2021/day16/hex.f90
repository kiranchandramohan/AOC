program mn
  implicit none
  type istack
    integer :: top = 0
    integer(kind=8) :: container(100)
  end type
  integer :: fd, ios
  character(len=20) :: inputFile = "input.txt"
  character(len=20) :: messageLengthChar
  character(len=:), allocatable :: message
  logical, allocatable :: binary(:)
  integer :: messageLength
  integer :: i, j, k
  integer :: totalVersion
  type(istack) :: stack

  totalVersion = 0
  call get_command_argument(1, inputFile)
  call get_command_argument(2, messageLengthChar)
  read(messageLengthChar,*) messageLength
  print *, inputFile, messageLength
  allocate(character(len=messageLength)::message)
  allocate(binary(4*messageLength))
  open(newunit=fd, file=inputFile, iostat=ios)
  read(fd,*) message
  print *, message
  close(fd)
  call convertToBinary(message)
  print *, extractPacket(binary,1,stack)
  print *, "Total = ", totalVersion
contains
  subroutine stackPush(s, v)
    type(istack) :: s
    integer(kind=8) :: v
    s%top = s%top + 1
    s%container(s%top) = v
  end subroutine
  function stackPop(s)
    type(istack) :: s
    integer :: stackPop
    stackPop = s%container(s%top)
    s%top = s%top - 1
  end function
  function evaluateStack(s, code)
    type(istack) :: s
    integer :: code
    integer(kind=8) :: evaluateStack
    select case (code)
    case (0)
            evaluateStack = SUM(s%container(1:s%top))
    case (1)
            evaluateStack = PRODUCT(s%container(1:s%top))
    case (2)
            evaluateStack = MINVAL(s%container(1:s%top))
    case (3)
            evaluateStack = MAXVAL(s%container(1:s%top))
    case (5)
            if (s%container(1) .gt. s%container(2)) then
              evaluateStack = 1
            else
              evaluateStack = 0
            end if
    case (6)
            if (s%container(1) .lt. s%container(2)) then
              evaluateStack = 1
            else
              evaluateStack = 0
            end if
    case (7)
            if (s%container(1) .eq. s%container(2)) then
              evaluateStack = 1
            else
              evaluateStack = 0
            end if
    end select
     !print *, "evaluate=", evaluateStack, s%top, "operation=", code
  end function
  recursive function extractPacket(b,s, stack) result(lastIndex)
    logical :: b(:)
    integer, value :: s
    integer :: k, i
    integer :: lengthPackets
    integer :: lastIndex, op, operation
    type(istack) :: stack, mystack
    !print *, "Packet :", s, size(b), b(s:size(b)-s+1)
    print *, "Version:", getDecimal(b, s, s+2)
    totalVersion = totalVersion + getDecimal(b, s, s+2)
    operation = getDecimal(b, s+3, s+5)
    if (operation .eq. 4) then
      print *, "Literal Packet"
      lastIndex = extractLiteral(b, s+6, stack)
    else
      op = getDecimal(b, s+6, s+6)
      print *, "Operator Packet", s
      if (op .eq. 1) then
        k=s+18
        print *, "Num subpackets = ", getDecimal(b, s+7, s+17)
        do i=1,getDecimal(b, s+7, s+17)
          lastIndex = extractPacket(b,k,mystack)
          k = lastIndex+1
        end do
        call stackPush(stack, evaluateStack(mystack, operation))
      else
        lengthPackets = getDecimal(b, s+7, s+21)
        print *, "Length of subpackets = ", lengthPackets
        lastIndex = s + 21
        do while (lengthPackets > lastIndex - (s + 21))
         lastIndex = extractPacket(b, lastIndex + 1, mystack)
        end do 
        call stackPush(stack, evaluateStack(mystack, operation))
      end if
    end if
    print *, "Packet Value = ", stack%top, stack%container(1)
  end function

  function extractLiteral(b, s, mystack)
    logical :: b(:)
    integer :: s, k, i
    logical :: lb(4000)
    integer :: extractLiteral
    type(istack) :: mystack
    k = s
    i = 1
    do
      lb(i:i+3) = b(k+1:k+4)
      if (getDecimal(b, k, k) .eq. 0) exit
      k = k + 5
      i = i + 4
    end do
    print *, lb(1:i+3)
    print *, "Literal = ", getDecimal(lb, 1, i+3)
    extractLiteral = k + 4
    call stackPush(mystack, getDecimal(lb, 1, i+3))
  end function
  subroutine convertToBinary(m)
    character(len=*) :: m
    integer :: i,j
    j=1
    do i=1,len(m)
      binary(j:j+3) = getCode(m(i:i))
      j = j+4
    end do
  end subroutine

  function getDecimal(b, s, e)
    logical :: b(:)
    integer :: s, e
    integer(kind=8) :: getDecimal
    integer(kind=8) :: tmp
    integer :: i
    getDecimal = 0
    tmp = 1
    do i=e,s,-1
      if (b(i)) then
        getDecimal = getDecimal + tmp * 1
      end if
      tmp = tmp * 2
    end do
  end function

  function getCode(c)
    character :: c
    logical :: getCode(4)
    select case (c)
    case ('0')
    getCode = (/.false., .false., .false., .false./)
    case ('1')
    getCode = (/.false., .false., .false., .true./)
    case ('2')
    getCode = (/.false., .false., .true., .false./)
    case ('3')
    getCode = (/.false., .false., .true., .true./)
    case ('4')
    getCode = (/.false., .true., .false., .false./)
    case ('5')
    getCode = (/.false., .true., .false., .true./)
    case ('6')
    getCode = (/.false., .true., .true., .false./)
    case ('7')
    getCode = (/.false., .true., .true., .true./)
    case ('8')
    getCode = (/.true., .false., .false., .false./)
    case ('9')
    getCode = (/.true., .false., .false., .true./)
    case ('A')                                     
    getCode = (/.true., .false., .true., .false./)
    case ('B')                                     
    getCode = (/.true., .false., .true., .true./)
    case ('C')                                     
    getCode = (/.true., .true., .false., .false./)
    case ('D')                                     
    getCode = (/.true., .true., .false., .true./)
    case ('E')                                     
    getCode = (/.true., .true., .true., .false./)
    case ('F')                                     
    getCode = (/.true., .true., .true., .true./)
    end select
  end function
end program
