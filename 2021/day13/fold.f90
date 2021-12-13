program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  integer(kind=8) :: totalPaths
  character(len=12) :: numPointsChar
  integer :: numPoints
  character(len=12) :: numFoldsChar
  integer :: numFolds
  integer, allocatable :: pointsMatrix(:,:)
  integer :: tmp1, tmp2, i, j
  character :: axis, assign
  character(len=12) :: foldLine
  integer :: counter
  integer :: maxX, maxY

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numPointsChar)
  call get_command_argument(3, numFoldsChar)
  read(numPointsChar,*) numPoints
  read(numFoldsChar,*) numFolds
  print *, inputFile, numPoints, numFolds
  
  counter = 0
  maxX = 0
  maxY = 0
  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numPoints
    read(fd,*) tmp1,tmp2
    if (tmp1 > maxX) then
            maxX = tmp1
    end if
    if (tmp2 > maxY) then
            maxY = tmp2
    end if
  end do
  close(fd)
  print *, maxY,maxX
  allocate(pointsMatrix(0:maxY,0:maxX))
  pointsMatrix = 0

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numPoints
    read(fd,*) tmp1,tmp2
    pointsMatrix(tmp2, tmp1) = 1
  end do
  do j=1,numFolds
    read(fd,'(A)') foldLine
    tmp2 = scan(foldLine,'=')
    axis = foldLine(1:tmp2-1)
    read(foldLine(tmp2+1:len(foldLine)),*) tmp1
    print *, axis, tmp1
    !call printPointsMatrix()
    if (axis == 'y') then
      do i=1,tmp1
        pointsMatrix(tmp1-i,:) = IOR(pointsMatrix(tmp1-i,:), pointsMatrix(tmp1+i,:))
        maxY = tmp1-1
      end do
    else
      do i=1,tmp1
        pointsMatrix(:,tmp1-i) = IOR(pointsMatrix(:,tmp1-i), pointsMatrix(:,tmp1+i))
        maxX = tmp1-1
      end do
    end if
  end do

  close(fd)
    print *, "After", maxY, maxX
    call printPointsMatrix()
  print *, sum(pointsMatrix(0:maxY,0:maxX))
contains
  subroutine printPointsMatrix()
    integer :: i, j
    do i=0,maxY
      do j=0, maxX
        write (*,'(I4)', advance="no") pointsMatrix(i,j)
      end do
      write(*,*) ""
    end do
  end subroutine
end program
