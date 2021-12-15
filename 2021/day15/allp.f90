program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  character(len=12) :: numPointsChar
  integer :: numPoints
  integer :: i, j, k
  integer(kind=8), allocatable :: distance(:,:)
  character(len=:), allocatable :: input

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numPointsChar)
  read(numPointsChar,*) numPoints
  print *, inputFile, numPoints
  allocate(distance(numPoints*numPoints,numPoints*numPoints))
  allocate(character(len=numPoints)::input)
  distance = 99999999

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numPoints
    read(fd,*) input
    do k=1,numPoints
      call setDistance(j-1,k,j,k,getIndex(input(k:k)))
      call setDistance(j+1,k,j,k,getIndex(input(k:k)))
      call setDistance(j,k-1,j,k,getIndex(input(k:k)))
      call setDistance(j,k+1,j,k,getIndex(input(k:k)))
    end do
  end do
  close(fd)
  !call printMatrix(distance)
      do k=1, numPoints*numPoints
  do i = 1, numPoints*numPoints
    do j= 1, numPoints*numPoints
        if (distance(i, j) .gt. (distance(i,k) + distance(k,j))) then
          distance(i, j) = distance(i,k) + distance(k,j)
        end if
      end do
    end do
  end do
  print *, distance(1, numPoints*numPoints)
contains
  function getIndex(x)
    character :: x
    integer :: getIndex
    getIndex = IACHAR(x) - IACHAR('0')
  end function
  subroutine setDistance(x1, y1, x2, y2, d)
    integer :: x1, y1, x2, y2, d
    if (isValidPoint(x1,y1)) then
      distance((x1-1)*numPoints + y1, (x2-1)*numPoints + y2) = d
    end if
  end
  function isValidPoint(x1,y1)
    integer :: x1, y1
    logical :: isValidPoint
    if (x1 .ge. 1 .and. x1 .le. numPoints .and. y1 .ge. 1 .and. y1 .le. numPoints) then
      isValidPoint = .true.
    else
      isValidPoint = .false.
    end if
  end function
  subroutine printMatrix(x)
    integer(kind=8) :: x(:,:)
    integer :: i, j
    do i=1,size(x,1)
      do j=1,size(x,1)
        write(*, '(I4)', advance="no") x(i,j)
      end do
      print *, ""
    end do
  end subroutine
end program
