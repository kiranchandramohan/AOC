program main
  character(len=32) :: inputFile = 'input.txt'
  character(len=12) :: numFishInputChar
  integer :: numFishInput
  character(len=12) :: numDaysChar
  integer :: numDays
  integer, allocatable :: fishes(:)

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numFishInputChar)
  call get_command_argument(3, numDaysChar)

  read(numFishInputChar,*) numFishInput
  read(numDaysChar,*) numDays
  !print *, inputFile, numFishInput, numDays
  allocate(fishes(numFishInput))
  !print *, processEntries1()
  print *, processEntries2()
contains
  function compute()
    integer :: compute
    integer :: newFishes
    integer, allocatable :: tmpFishes(:)
    newFishes = 0
    !print *, "Day ", newFishes, fishes
    do i=1,numDays
      newFishes = 0
      do j=1,size(fishes)
        if (fishes(j) .eq. 0) then
          newFishes = newFishes + 1
          fishes(j) = 7
        end if
        fishes(j) = fishes(j) - 1
      end do
      fishes = reshape(fishes, shape(fishes) + newFishes, (/8/))
      !allocate(tmpFishes(size(fishes) + newFishes))
      !tmpFishes = 8
      !tmpFishes = fishes
      !print *, "Tmp ", i, tmpFishes
      !fishes = tmpFishes
      !print *, "Day ", i, fishes
    end do
    compute = size(fishes)
  end function

  function processEntries1()
    integer :: processEntries1
    integer :: fd
    open(newunit=fd, file=inputFile, iostat=ios)
    read(fd, *) fishes
    close(fd)
    print *, fishes
    processEntries1 = compute()
  end function

  function processEntries2()
    integer(kind=8) :: processEntries2
    integer :: fd
    integer(kind=8) :: fishCount(9)
    integer(kind=8) :: newFishes
    open(newunit=fd, file=inputFile, iostat=ios)
    read(fd, *) fishes
    close(fd)
    !print *, fishes

    fishCount = 0

    do i = 1, size(fishes)
      fishCount(fishes(i)+1) = fishCount(fishes(i)+1) + 1
    end do

    !print *, fishCount
    !print *, SUM(fishCount)

    do i =1, numDays
      newFishes = fishCount(1)
      do j=1,8
        fishCount(j) = fishCount(j+1)
      end do
      fishCount(9) = newFishes
      fishCount(7) = fishCount(7) + newFishes
    end do

    !print *, fishCount

    processEntries2 = SUM(fishCount)
  end function
end program
