program main
  character(len=32) :: inputFile = 'input.txt'
  character(len=12) :: numLineChar, boardMaxChar
  integer :: numLine, boardMax
  integer, allocatable :: board(:, :)

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numLineChar)
  call get_command_argument(3, boardMaxChar)
  read(numLineChar,*) numLine
  read(boardMaxChar,*) boardMax
  print *, inputFile, numLine, boardMax

  allocate(board(boardMax,boardMax))
  board = 0
  print *, processEntries1()
contains
  subroutine updateBoard(x1, y1, x2, y2)
    integer :: x1, y1, x2, y2
    integer :: step
    integer :: stepx, stepy
    integer :: nx, ny
    integer :: diff
    !print *, x1, y1, x2, y2
    !call printBoard()
    if (x1 == x2) then
      if (y2 .ge. y1) then
        step =1
      else
        step = -1
      end if
      do i=y1,y2,step
        board(x1,i) = board(x1,i) + 1
      end do
    else if(y1 == y2) then
      if (x2 .ge. x1) then
        step =1
      else
        step = -1
      end if
      do i=x1,x2,step
        board(i,y1) = board(i,y1) + 1
      end do
    else
      if (x2 .ge. x1) then
        stepx = 1
      else
        stepx = -1
      end if 
      if (y2 .ge. y1) then
        stepy = 1
      else
        stepy = -1
      end if 
      diff = abs(x2 - x1)
      do i=0,diff
        nx = x1 + i*stepx
        ny = y1 + i*stepy
        board(nx, ny) = board(nx,ny) + 1
      end do
    end if
    !print *, x1, y1, x2, y2
    !call printBoard()
    !print *, "---------------------------------------"
  end subroutine

  subroutine printBoard()
    do i = 1, boardMax
      print *, board(i,:)
    end do
  end subroutine

  function reportBoard()
    integer :: reportBoard
    reportBoard = 0
    do i=1, boardMax
    do j=1, boardMax
      if (board(i,j) .ge. 2) then
        reportBoard = reportBoard + 1
      end if
    end do
    end do
  end function

  subroutine collectElems()
    integer :: fd
    integer :: x1, y1, x2, y2
    character(len=2) :: impl
    open(newunit=fd, file=inputFile, iostat=ios)
    do i=1,numLine
      read(fd, *) x1, y1, impl, x2, y2
      !if ((x1 == x2) .or. (y1 == y2)) then
        call updateBoard(x1+1, y1+1, x2+1, y2+1)
      !end if
    end do
    close(fd)
  end subroutine

  function processEntries1()
    integer :: i
    integer :: processEntries1
    call collectElems()
    processEntries1 = reportBoard()
  end function
end program
