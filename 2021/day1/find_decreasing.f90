program main
  integer, allocatable :: entries(:)
  integer :: numEntries
  CHARACTER(len=32) :: inputFile

  call get_command_argument(1, inputFile)
  numEntries = countEntries()
  allocate(entries(numEntries))
  call collectEntries(numEntries, entries)
  print *, findIncreasing(numEntries, entries)
  print *, findIncreasingWindows(numEntries, entries)
contains
  function countEntries() result(numEntries)
    integer :: numEntries
    numEntries = 0
    open(1, file = inputFile)
    do
      read(1,*,iostat=io)
      if(io/=0) exit
      numEntries = numEntries + 1
    end do
    close(1)
  end function

  subroutine collectEntries(n, entries)
    integer :: n
    integer, intent(inout) :: entries(:)
    integer :: i
    integer :: fd

    open(newunit=fd, file=inputFile, iostat=ios)
    do i=1,n
      read(fd, *) entries(i)
    end do
    close(fd)
  end subroutine

  function findIncreasing(n, entries) result(incrs)
    integer :: n
    integer, intent(inout) :: entries(:)
    integer :: incrs
    incrs = 0
    do i=1,n-1
      if (entries(i+1)>entries(i)) then
        incrs = incrs + 1
      end if
    end do
  end function

  function findIncreasingWindows(n, entries) result(incrs)
    integer :: n
    integer, intent(inout) :: entries(:)
    integer :: incrs
    window(i) = entries(i) + entries(i-1) + entries(i-2)
    incrs = 0
    do i=3,n-1
      if (window(i+1)>window(i)) then
        incrs = incrs + 1
      end if
    end do
  end function
end program
