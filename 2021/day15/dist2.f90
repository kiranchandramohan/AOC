
program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  character(len=12) :: numPointsChar
  integer :: numPoints
  character(len=12) :: origPointsChar
  integer :: origPoints
  integer :: i, j, k
  integer(kind=4), allocatable :: length(:,:)
  integer(kind=4), allocatable :: dist(:)
  integer(kind=4), allocatable :: remVert(:)
  integer(kind=4), allocatable :: initData(:,:)
  logical, allocatable :: isInQ(:)
  integer :: remVertSize
  integer :: startVert
  character(len=:), allocatable :: input(:)
  integer :: curX, curY, tmpXY, tmpX, tmpY, curMin

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numPointsChar)
  call get_command_argument(3, origPointsChar)
  read(numPointsChar,*) numPoints
  read(origPointsChar,*) origPoints
  print *, inputFile, numPoints
  allocate(length(numPoints*numPoints,numPoints*numPoints))
  allocate(isInQ(numPoints*numPoints))
  allocate(dist(numPoints*numPoints))
  allocate(remVert(numPoints*numPoints))
  allocate(character(len=origPoints)::input(origPoints))
  allocate(initData(numPoints,numPoints))
  length = 99999999
  dist = 99999999
  remVertSize = 0
  startVert = 0

  isInQ = .true.
  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,origPoints
    read(fd,*) input(j)
    do k=1,origPoints
      initData(j,k) = getIndex(input(j)(k:k))
    end do
  end do
  close(fd)
  do j=origPoints+1,numPoints
    do k=1,origPoints
      initData(j,k) = getNextVal(initData(j-origPoints,k))
    end do
  end do
  do j=1,numPoints
    do k=origPoints+1,numPoints
      initData(j,k) = getNextVal(initData(j,k-origPoints))
    end do
  end do
  do j=1,numPoints
    do k=1,numPoints
      !print *, "Updating points around ", j , k
      call setLength(j-1,k,j,k,initData(j,k))
      call setLength(j+1,k,j,k,initData(j,k))
      call setLength(j,k-1,j,k,initData(j,k))
      call setLength(j,k+1,j,k,initData(j,k))
    end do
  end do

  !call printMatrix(length)

  dist(1) = 0
  do i=1, numPoints
    do j=1, numPoints
      call addVert(i,j)
    end do
  end do

  !print *, remVert

  do while (remVertSize .ge. 1)
    tmpXY = getMin()
    tmpX = (tmpXY-1)/numPoints + 1
    tmpY = mod(tmpXY-1,numPoints) + 1
    !print *, "Current Point = ", tmpX, tmpY, tmpXY, dist(tmpXY)
    isInQ(tmpXY) = .false.
    call updateNeighbours(tmpX, tmpY, dist(tmpXY))
  end do
  print *, dist(numPoints*numPoints)
contains
  function getNextVal(x)
          integer(kind=4) :: getNextVal
          integer(kind=4) :: x
          getNextVal = x + 1
          if (getNextVal .eq. 10) then
             getNextVal = 1
          end if
  end function
  subroutine addVert(x,y)
    integer :: x, y
    remVertSize = remVertSize + 1
    remVert(remVertSize) = (x-1)*numPoints+y
  end subroutine


  function getMin()
    integer :: i
    integer(kind=4) :: minValue
    integer :: minIdx, minI, getMin
    minValue = 99999999
    do i = 1, remVertSize
      if (dist(remVert(i)) < minValue) then
        minValue = dist(remVert(i))
        minIdx = remVert(i)
        minI = i
      end if
    end do
    remVert(minI) = remVert(remVertSize)
    remVertSize = remVertSize - 1
    getMin = minIdx
  end function

  function getIndex(x)
    character :: x
    integer :: getIndex
    getIndex = IACHAR(x) - IACHAR('0')
  end function

  subroutine updateNeighbours(x1,y1, d)
    integer :: x1, y1
    integer(kind=4) :: d
    !print *, "updateNeighbour", x1-1, y1
    call updateNeighbour(x1-1, y1, x1, y1, d)
    !print *, "updateNeighbour", x1+1, y1
    call updateNeighbour(x1+1, y1, x1, y1, d)
    !print *, "updateNeighbour", x1, y1-1
    call updateNeighbour(x1, y1-1, x1, y1, d)
    !print *, "updateNeighbour", x1, y1+1
    call updateNeighbour(x1, y1+1, x1, y1, d)
  end subroutine

  subroutine updateNeighbour(x1,y1, curX, curY, d)
    integer :: x1, y1, curX, curY
    integer(kind=4) :: d, cur
    integer :: current, neighbour
    current = (curX-1)*numPoints + curY
    neighbour = (x1-1)*numPoints + y1
    if (isValidPoint(x1,y1)) then
      cur = dist(neighbour)
      if ( d + length(current, neighbour) < cur) then
        dist(neighbour) = d + length(current, neighbour)
      end if
      !print *, "updateNeighbour", x1, y1, d, dist(neighbour), length(current,neighbour)
    end if
  end subroutine

  function isValidPoint(x1,y1)
    integer :: x1, y1
    logical :: isValidPoint
    if (x1 .ge. 1 .and. y1 .ge. 1 .and. x1 .le. numPoints .and. y1 .le. numPoints .and. isInQ((x1-1)*numPoints+y1)) then
      isValidPoint = .true.
      !print *, "Valid point ",  x1 , y1
    else
      !print *, "Invalid point ",  x1 , y1
      isValidPoint = .false.
    end if
  end function
  subroutine printMatrix(x)
    integer(kind=4) :: x(:,:)
    integer :: i, j
    do i=1,size(x,1)
      do j=1,size(x,1)
        if (x(i,j) .le. 9) then
        print *, i, j, x(i,j)
        end if
      end do
    end do
      !do j=1,size(x,1)
      !  if( x(i,j) .eq. 99999999) then
      !  write(*, '(I4)', advance="no") 9999
      !  else
      !  write(*, '(I4)', advance="no") x(i,j)
      !  end if
      !end do
      !print *, ""
  end subroutine

 subroutine setLength(x1, y1, x2, y2, d)
    integer :: x1, y1, x2, y2, d
    if (isValidPoint(x1,y1) .eqv. .true.) then
      length((x1-1)*numPoints + y1, (x2-1)*numPoints + y2) = d
      !print *, "length = ", (x1-1)*numPoints + y1, (x2-1)*numPoints + y2, length((x1-1)*numPoints + y1, (x2-1)*numPoints + y2), d
    end if
  end
end program
