program mn
  character(len=32) :: inputFile = 'input.txt'
  character(len=12) :: numSubInputChar
  integer :: numSubInput
  integer, allocatable :: subs(:)
  integer, allocatable :: cost(:,:)
  integer :: fd
  integer :: mval
  call get_command_argument(1, inputFile)
  call get_command_argument(2, numSubInputChar)
  read(numSubInputChar,*) numSubInput
  print *, inputFile, numSubInput

  allocate(subs(numSubInput))
  open(newunit=fd, file=inputFile, iostat=ios)
  read(fd, *) subs
  close(fd)
  print *, subs
  mval = maxval(subs)
  allocate(cost(mval,0:mval))
  do j=0,mval
    cost(1,j) = sumton(abs(subs(1) - j))
  end do
  do i=2,size(subs)
    do j=0,mval
      cost(i,j) = sumton(abs(subs(i) - j)) + cost(i-1,j)
    end do
  end do
  !do i=1,size(subs)
  !  print *, cost(i,:)
  !end do
  print *, minval(cost(size(subs),:))
contains
    function sumton(x)
    integer :: sumton
    integer :: x
    sumton = (x * (x+1))/2
    end function
end program
