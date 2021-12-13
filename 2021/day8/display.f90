program mn
  implicit none
  character(len=32) :: inputFile = 'input.txt'
  character(len=12) :: numSubInputChar
  integer :: numSubInput
  character(len=7) :: output(4)
  character(len=7) :: input(10)
  integer :: vals(10)
  integer :: mapValToInput(0:9)
  character :: tmp
  integer :: fd
  integer :: i, j, k, ios
  integer :: tmp_length
  integer :: total_unique
  integer(kind=8) :: curSum
  integer(kind=8) :: totalSum
  integer(kind=8) :: grandSum
  grandSum = 0
  call get_command_argument(1, inputFile)
  call get_command_argument(2, numSubInputChar)
  read(numSubInputChar,*) numSubInput
  print *, inputFile, numSubInput

  total_unique = 0
  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numSubInput
  curSum = 0
  totalSum = 0
    read(fd, *) input, tmp, output

    vals = -1
    mapValToInput = -1
    do i=1,10
      tmp_length = len_trim(input(i))
      select case(tmp_length)
      case(2)
      !1 (ab)
      vals(i) = 1
      mapValToInput(1) = i
      case(4)
      !4 (abef)
      vals(i) = 4
      mapValToInput(4) = i
      case(3)
      !7 (abd)
      vals(i) = 7
      mapValToInput(7) = i
      case(7)
      !8 (abcdefg)
      vals(i) = 8
      mapValToInput(8) = i
      end select
    end do
    do i=1,10
      tmp_length = len_trim(input(i))
      select case(tmp_length)
      case(6)
      !0,6,9 (abcdeg) (bcdefg) (abcdef)
      if (substring(input(i), input(mapValToInput(4)))) then
        vals(i) = 9
        mapValToInput(9) = i
      end if
      if (.not. substring(input(i), input(mapValToInput(7)))) then
        vals(i) = 6
        mapValToInput(6) = i
      end if
      case(5)
      !2,3,5 (acdfg) (abcdf) (bcdef)
      if (substring(input(i), input(mapValToInput(7)))) then
        vals(i) = 3
        mapValToInput(3) = i
      end if
      end select
    end do
    do i=1,10
      tmp_length = len_trim(input(i))
      select case(tmp_length)
      case(6)
      !0,6,9 (abcdeg) (bcdefg) (abcdef)
      if (vals(i) .eq. -1) then
        vals(i) = 0
        mapValToInput(0) = i
      end if
      case(5)
      !2,3,5 (acdfg) (abcdf) (bcdef)
      if (substring(input(mapValToInput(6)),input(i))) then
        vals(i) = 5
        mapValToInput(5) = i
      end if
      end select
    end do
    do i=1,10
      tmp_length = len_trim(input(i))
      select case(tmp_length)
      case(5)
      if (vals(i) .eq. -1) then
        vals(i) = 2
        mapValToInput(2) = i
      end if
      end select
    end do

    !print *, vals
    !print *, mapValToInput

    do i=1,4
    do k=1,10
      if (same(trim(output(i)),trim(input(k)))) then
         curSum = vals(k)
         exit
      end if
    end do
    totalSum = 10*totalSum + curSum
    end do
    grandSum = grandSum + totalSum
  end do
  close(fd)
  print *, grandSum
contains
  function same(x,y)
     character(len=*) :: x
     character(len=*) :: y
     logical :: same
     same = substring(x,y) .and. substring(y,x)
  end function
  function substring(x,y)
     character(len=*) :: x
     character(len=*) :: y
     logical :: found
     logical :: substring
     integer :: i, j
     do i=1,len(y)
       found = .false.
       do j=1, len(x)
         if(y(i:i) == x(j:j)) then
           found = .true.
         end if
       end do
       if (.not. found) then
          exit
       end if
     end do
     substring = found
  end function
end program
