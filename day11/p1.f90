program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  integer :: i, j
  integer(kind=8) :: totalFlash
  character(len=12) :: numInput1Char
  integer :: numInput1
  character(len=12) :: numStepsChar
  integer :: numSteps
  integer, allocatable :: energy(:,:)
  integer, allocatable :: updateEnergyList(:,:)
  character(len=10) :: inp

  totalFlash = 0

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numInput1Char)
  call get_command_argument(3, numStepsChar)
  read(numInput1Char,*) numInput1
  read(numStepsChar,*) numSteps
  print *, inputFile, numInput1, numSteps

  allocate(energy(numInput1,numInput1))
  allocate(updateEnergyList(numInput1, numInput1))

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numInput1
    read(fd,*) inp
    do i=1, len_trim(inp)
      energy(j,i) = iachar(inp(i:i)) - iachar('0')
    end do
  end do
  close(fd)
  do j=1,numInput1
    print *, energy(j,:)
  end do

  do i=1, numSteps
    call doStep()
    if (ALL(energy .eq. 0)) then
       print *, "All zero at step", i
       exit
    end if
  end do
  print *, ""
  do j=1,numInput1
    print *, energy(j,:)
  end do
  print *, totalFlash
contains
  subroutine doStep()
    integer :: i, j

    energy = energy + 1

    do
      updateEnergyList = 0
      do i=1, numInput1
        do j=1, numInput1
          if (energy(i, j) .gt. 9) then
            totalFlash = totalFlash + 1
            call updateList(i, j)
            energy(i, j) = -1
          end if
        end do
      end do

      if (.not. ANY(updateEnergyList .gt. 0)) exit

      where (energy .le. 9 .and. energy .ne. -1) energy = energy + updateEnergyList

      where (energy .eq. -1) energy = 0
    end do
  end subroutine

  subroutine updateList(x, y)
    integer x, y
    print *, "updates for (",x,",",y,")"
    call update(x-1, y-1)
    call update(x+1, y+1)
    call update(x+1, y-1)
    call update(x-1, y+1)
    call update(x, y-1)
    call update(x, y+1)
    call update(x-1, y)
    call update(x+1, y)
    print *, "------------------------"
  end subroutine

  subroutine update(x, y)
    integer :: x, y
    if (validPoint(x, y)) then
      updateEnergyList(x, y) = updateEnergyList(x, y) + 1
      print *, "updating", x, y, updateEnergyList(x, y)
    end if
  end subroutine

  function validPoint(x, y)
    integer :: x, y
    logical :: validPoint
    validPoint = .false.

    if ( x .ge. 1 .and. y .ge. 1 .and. x .le. numInput1 .and. y .le. numInput1 .and. energy(x,y) .ne. -1) then
      validPoint = .true.
    end if
  end function
end program mn
