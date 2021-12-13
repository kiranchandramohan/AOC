program main
  integer :: numEntries
  character(len=32) :: inputFile

  call get_command_argument(1, inputFile)
  print *, processEntries1()
  print *, processEntries2()
contains
  function processEntries1()
    integer :: processEntries1
    integer :: dirValue
    character(len=32) :: dir
    integer :: fd
    integer :: horizontal = 0, depth = 0

    open(newunit=fd, file=inputFile, iostat=ios)
    do
      read(fd, *, iostat=io) dir, dirValue
      if (io /= 0) exit
      if (dir == "forward") then
        horizontal = horizontal + dirValue
      elseif (dir == "up") then
        depth = depth - dirValue
      else
        depth = depth + dirValue
      endif
      !print *, horizontal, depth
    end do
    close(fd)
    processEntries1 = horizontal * depth
  end function

  function processEntries2()
    integer :: processEntries2
    integer :: dirValue
    character(len=32) :: dir
    integer :: fd
    integer :: horizontal = 0, depth = 0, aim = 0

    open(newunit=fd, file=inputFile, iostat=ios)
    do
      read(fd, *, iostat=io) dir, dirValue
      if (io /= 0) exit
      if (dir == "forward") then
        horizontal = horizontal + dirValue
        depth = depth + (aim * dirValue)
      elseif (dir == "up") then
        aim = aim - dirValue
      else
        aim = aim + dirValue
      endif
      !print *, horizontal, depth, aim
    end do
    close(fd)
    processEntries2 = horizontal * depth
  end function
end program
