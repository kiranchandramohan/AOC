program main
  integer :: numEntries
  character(len=32) :: inputFile = 'input.txt'

  call get_command_argument(1, inputFile)
  print *, processEntries1()
  print *, processEntries2()
contains
  subroutine collectElems(x)
    character(len=12), allocatable :: x(:)
    integer :: n
    character(len=12) :: val
    integer :: fd
    n = 0
    open(newunit=fd, file=inputFile, iostat=ios)
    do
      read(fd, *, iostat=io) val
      if (io /= 0) exit
      n = n + 1
    end do
    allocate(x(n))
    close(fd)

    n = 0
    open(newunit=fd, file=inputFile, iostat=ios)
    do
      read(fd, *, iostat=io) val
      if (io /= 0) exit
      n = n + 1
      x(n) = val
    end do
    close(fd)
  end subroutine

  function collectElemsWithChar(x, pos, c)
    character(len=12), allocatable :: x(:)
    integer :: pos
    character :: c
    character(len=12), allocatable :: collectElemsWithChar(:)
    integer :: nElems ,j, i
    nElems = 0

    do i=1, size(x)
      if (x(i)(pos:pos) == c) then
        nElems = nElems + 1
      end if
    end do

    allocate(collectElemsWithChar(nElems))
    j = 1
    do i=1, size(x)
      if (x(i)(pos:pos) == c) then
        collectElemsWithChar(j) = x(i)
        j = j + 1
      end if
    end do

  end function

  subroutine fillZerosOnesInfo(zeros, ones, x)
    integer :: zeros(12)
    integer :: ones(12)
    character(len=12) :: x(:)
    integer :: length
    length = 0
    zeros = 0
    ones = 0 
    do i=1,size(x)
      !print *, x(i), " ", len_trim(x(i))
      length = len_trim(x(i))
      do j=1,length
        if (x(i)(j:j) == '0') then
          zeros(j) = zeros(j) + 1
        else
          ones(j) = ones(j) + 1
        end if
      end do
    end do
  end subroutine

  function computeGE(zeros,ones, length)
    integer :: zeros(12)
    integer :: ones(12)
    integer :: length
    integer :: g =0, e=0
    integer :: computeGE

    do j=1,length
      if (zeros(j) > ones(j)) then
        g = 2*g+0
        e = 2*e+1
      else
        g = 2*g+1
        e = 2*e+0
      endif
    end do
    computeGE = g*e
  end function

  function convertToDecimal(x)
    integer :: convertToDecimal
    character(len=12) :: x
    convertToDecimal = 0
    do j=1,len_trim(x)
     if (x(j:j) == '0') then
       convertToDecimal = 2*convertToDecimal + 0
     else
       convertToDecimal = 2*convertToDecimal + 1
    end if
    end do
  end function

  function processEntries1()
    integer :: processEntries1
    integer :: fd
    integer :: zeros(12)
    integer :: ones(12)
    character(len=12), allocatable :: x(:)

    call collectElems(x)

    call fillZerosOnesInfo(zeros, ones, x)
    processEntries1 = computeGE(zeros, ones, len_trim(x(1)))

  end function

  function processEntries2()
    integer :: processEntries2
    integer :: fd
    integer :: zeros(12)
    integer :: ones(12)
    character(len=12), allocatable :: x(:)
    character(len=12), allocatable :: y(:)
    numElems = 0
    processEntries2 = 0

    call collectElems(x)

    y = x
    do j=1,len_trim(x(1))
      if (size(y) == 1) exit
      call fillZerosOnesInfo(zeros, ones, y)
      if (ones(j) >= zeros(j)) then
        y = collectElemsWithChar(y, j, '1')
      else
        y = collectElemsWithChar(y, j, '0')
      endif
    end do
    processEntries2 = convertToDecimal(y(1))

    y = x
    do j=1,len_trim(y(1))
      if (size(y) == 1) exit
      call fillZerosOnesInfo(zeros, ones, y)
      if (zeros(j) <= ones(j)) then
        y = collectElemsWithChar(y, j, '0')
      else
        y = collectElemsWithChar(y, j, '1')
      endif
    end do
    processEntries2 = processEntries2 * convertToDecimal(y(1))
  end function
end program
