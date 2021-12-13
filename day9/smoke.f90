program mn
  implicit none
  character(len=32) :: inputFile = 'input.txt'
  character(len=12) :: numInput1Char
  integer :: numInput1
  character(len=12) :: numInput2Char
  integer :: numInput2
  integer, allocatable :: heights(:,:)
  integer :: numBasins
  integer(kind=8) :: curSum
  integer(kind=8) :: totalSum
  integer(kind=8) :: grandSum
  integer :: fd, ios, i, j
  character(len=:), allocatable :: line
  integer, allocatable :: basins(:)
  integer :: sizes(3)
  logical, allocatable :: processed(:,:)
  sizes = -1

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numInput1Char)
  call get_command_argument(3, numInput2Char)
  read(numInput1Char,*) numInput1
  read(numInput2Char,*) numInput2
  print *, inputFile, numInput1, numInput2
  allocate(heights(numInput1,numInput2))
  allocate(character(len=numInput2) :: line)

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numInput1
    read(fd,*) line
    do i=1,len(line)
      heights(j,i) = IACHAR(line(i:i)) - IACHAR('0')
    end do
  end do
  close(fd)
  do j=1,numInput1
    print *, heights(j,:)
  end do
  curSum = 0
  totalSum = 0
  numBasins = 0
  allocate(basins(240))
  allocate(processed(numInput1,numInput2))
  do i=1, numInput1
    do j=1, numInput2
      if (checkLowPoint(i,j)) then
        processed = .false.
        curSum = curSum + heights(i,j) + 1
        numBasins = numBasins + 1
        totalSum = totalSum + updateSizes(sizeBasin(i,j,-1))
        print *, i, j, checkLowPoint(i,j), totalSum
      end if
    end do
  end do
  print *, curSum
  print *, numBasins
  print *, totalSum
  print *, sizes, sizes(1) * sizes(2) * sizes(3)
contains
   function updateSizes(s)
     integer :: s, updateSizes
     integer :: cur, tmp
     integer :: i
     cur = s
     do i=1,3
       if (cur > sizes(i)) then
         tmp = sizes(i)
         sizes(i) = cur
         cur = tmp
       end if
     end do
     updateSizes = s
   end function
   recursive function sizeBasin(x,y,ph) result(res)
      integer :: res
      integer :: x, y, ph
      integer :: a, b, c, d
      a = 0
      b = 0
      c = 0
      d = 0
      res = 0
      if ( x .ge. 1 .and. a .le. numInput1 .and. y .ge. 1 .and. y .le. numInput2 .and. (.not. processed(x,y)) .and. heights(x,y) .ne. 9 .and. heights(x,y) .gt. ph) then
        processed(x,y) = .true.
        a = sizeBasin(x,y+1,heights(x,y))
        b = sizeBasin(x,y-1,heights(x,y))
        c = sizeBasin(x+1,y,heights(x,y))
        d = sizeBasin(x-1,y,heights(x,y))
        res = a + b + c + d + 1
      end if
   end function
   function checkLowPoint(x,y)
      integer :: x,y
      logical :: checkLowPoint
      integer :: left, right, up, down

      left = 10
      right = 10
      up = 10
      down = 10
      if (x > 1) then
        up = heights(x-1,y)
      end if
      if (y > 1) then
        left = heights(x,y-1)
      end if
      if (x < numInput1) then
        down = heights(x+1,y)
      end if
      if (y < numInput2) then
        right = heights(x,y+1)
      end if

      checkLowPoint = .false.
      if(heights(x,y) < up .and. heights(x,y) < down .and. heights(x,y) < left .and. heights(x,y) < right) then
        checkLowPoint = .true.
      end if
   end function
end program
