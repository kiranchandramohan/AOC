program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  integer(kind=8) :: totalChars
  character :: maxChar
  integer(kind=8) :: maxCharCount
  character :: minChar
  integer(kind=8) :: minCharCount
  character(len=12) :: numProductionsChar
  integer :: numProductions
  character(len=12) :: numStepsChar
  integer :: numSteps
  character(len=20) :: input
  character(len=2) :: lhs
  character(len=2) :: hyphen
  character :: rhs
  integer :: i, j, k
  integer(kind=8) :: countChar(26,26)
  integer(kind=8) :: tmpChar(26,26)
  integer(kind=8) :: production(26,26)
  integer(kind=8) :: sums(26)
  integer :: startChar
  countChar = 0
  production = 0

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numProductionsChar)
  call get_command_argument(3, numStepsChar)
  read(numProductionsChar,*) numProductions
  read(numStepsChar,*) numSteps
  print *, inputFile, numProductions, numSteps
  open(newunit=fd, file=inputFile, iostat=ios)
  read(fd,*) input
  print *, input, len_trim(input)
  startChar = getIndex(input(1:1))
  do i = 1, len_trim(input) - 1
    countChar(getIndex(input(i:i)), getIndex(input(i+1:i+1))) = 1
  end do
  call printMatrix(countChar)
  do j=1,numProductions
    read(fd,*) lhs, hyphen, rhs
    print *, lhs, hyphen, rhs
    production(getIndex(lhs(1:1)),getIndex(lhs(2:2))) =  getIndex(rhs)
  end do
  close(fd)
  call printMatrix(production)
  do k=1, numSteps
    tmpChar = 0
    do i = 1, 26
      do j= 1, 26
        if (countChar(i, j) .gt. 0) then
          tmpChar(i, production(i,j)) =  tmpChar(i, production(i,j)) + countChar(i, j)
          tmpChar(production(i,j), j) =  tmpChar(production(i,j), j) + countChar(i, j)
        end if
      end do
    end do
    countChar = tmpChar
  end do
  sums = SUM(countChar,1)
  sums(startChar) = sums(startChar) + 1
  print *, sums
  print *, MAXVAL(sums) - MINVAL(sums,sums.ge.1)
contains
  function getIndex(x)
    character :: x
    integer :: getIndex
    getIndex = IACHAR(x) - IACHAR('A') + 1
    !print *, getIndex, x
  end function
  subroutine printMatrix(x)
    integer(kind=8) :: x(26,26)
    do i=1,26
      do j=1,26
        write(*, '(I4)', advance="no") x(i,j)
      end do
      print *, ""
    end do
  end subroutine
end program
