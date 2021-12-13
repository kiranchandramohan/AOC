program main
  type board
    integer :: val(5,5)
    logical :: marked(5,5) = .false.
  end type
  character(len=32) :: inputFile = 'input.txt'
  character(len=:), allocatable :: input
  integer, allocatable :: inputs(:)
  type(board), allocatable :: boards(:)
  character(len=32) :: numBoardsTmp
  integer :: numBoards
  logical, allocatable :: boardStatus(:)

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numBoardsTmp)
  read(numBoardsTmp, *, iostat=ios) numBoards
  allocate(boards(numBoards))
  allocate(boardStatus(numBoards))
  boardStatus = .false.
  call collectElems()
  print *, processEntries1()
  print *, processEntries2()
contains
  subroutine fillCharSepArray(l, c, a)
    character(len=*) :: l
    character :: c
    integer :: a(:)
    integer :: j, n, offset
    j = 0
    do
      j = j + 1
      n = len(l)
      offset = index(l,",")
      if (offset == 0) then 
        read(l(1:n),*,iostat=ios) a(j)
        exit
      else
        read(input(1:offset),*,iostat=ios) a(j)
        l = l(offset+1:n)
      endif
    end do
  end subroutine
  subroutine collectElems()
    integer :: n
    integer :: intval
    integer :: nElems
    integer :: fd
    integer :: offset
    inquire(file=inputFile, size=n)
    open(newunit=fd, file=inputFile, iostat=ios)
    allocate(character(len=n)::input)
    read(fd, '(A)') input
    input = trim(input)
    nElems = 0
    do i=1,n
      if (input(i:i) == ',') then
        nElems = nElems + 1
      endif
    end do
    allocate(inputs(nElems+1))
    call fillCharSepArray(input, ',', inputs)
    do i = 1, numBoards
      read(fd, '(A)') input
      do j = 1, 5
        read(fd, *, iostat=io) boards(i)%val(j,:)
      end do
    end do
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

  function checkBoard(b)
    logical :: checkBoard
    type(board) :: b
    do i = 1, 5
      if (all(b%marked(i,:)) .or. all(b%marked(:,i))) then
        checkBoard = .true.
        return
      end if
    end do
    checkBoard = .false.
  end function

  function computeBoardVal(b)
    integer :: computeBoardVal
    type(board) :: b
    computeBoardVal = 0
    do i = 1, 5
      do j = 1, 5
        if ( .not. b%marked(i,j)) then
          computeBoardVal = computeBoardVal + b%val(i,j)
        end if
      end do
    end do
  end function

  function processEntries1()
    integer :: i
    integer :: processEntries1

    boardDo: do i=1, size(inputs)
      print *, inputs(i)
      do j=1, numBoards
        do k=1,5
          do l=1,5
            if (boards(j)%val(k,l) == inputs(i)) then
              boards(j)%marked(k,l) = .true.
            end if
          end do
        end do
      end do

      do j=1, numBoards
        if (checkBoard(boards(j))) then
          processEntries1 = computeBoardVal(boards(j)) * inputs(i)
          exit boardDo
        end if
      end do
    end do boardDo
  end function

  function processEntries2()
    integer :: i
    integer :: processEntries2

    boardDo: do i=1, size(inputs)
      print *, inputs(i)
      do j=1, numBoards
        do k=1,5
          do l=1,5
            if (boards(j)%val(k,l) == inputs(i)) then
              boards(j)%marked(k,l) = .true.
            end if
          end do
        end do
      end do

      do j=1, numBoards
        if (checkBoard(boards(j))) then
          boardStatus(j) = .true.
          if (all(boardStatus)) then
            print *, "Finish = ", j
            processEntries2 = computeBoardVal(boards(j)) * inputs(i)
            exit boardDo
          end if
        end if
      end do
    end do boardDo
  end function
end program
