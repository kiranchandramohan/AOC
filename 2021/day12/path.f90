program mn
  implicit none
  integer :: fd, ios
  character(len=12) :: inputFile = "input.txt"
  integer :: i, j
  integer(kind=8) :: totalPaths
  character(len=12) :: numEdgeChar
  integer :: numEdge
  character(len=20) :: v1Char
  character(len=20) :: v2Char
  character(len=20), allocatable :: input(:)
  character(len=20), allocatable :: modInput(:)
  character(len=20) :: tmp1String
  character(len=20) :: tmp2String
  integer :: path(8)
  character :: hyphen
  integer :: vCounter
  integer, allocatable :: adjMatrix(:,:)
  logical :: isBig(100)
  logical :: visited(100)
  integer :: sIndex
  integer :: eIndex

  totalPaths = 0
  vCounter = 0
  isBig = .false.
  visited = .false.

  call get_command_argument(1, inputFile)
  call get_command_argument(2, numEdgeChar)
  read(numEdgeChar,*) numEdge
  print *, inputFile, numEdge
  allocate(input(numEdge))
  allocate(modInput(numEdge))

  open(newunit=fd, file=inputFile, iostat=ios)
  do j=1,numEdge
    read(fd,'(A)') input(j)
    print *, input(j)
  end do
  close(fd)
  print *, input
  do j=1,numEdge
    call readEdge(input(j))
    if(notAnumber(v1Char)) then
      vCounter = vCounter + 1
      if(v1Char(1:1) >= 'A' .and. v1Char(1:1) <= 'Z') then
         isBig(vCounter) = .true.
      end if
      call renameVertex(j, vCounter, v1char)
    end if
    if(notAnumber(v2Char)) then
      vCounter = vCounter + 1
      if(v2Char(1:1) >= 'A' .and. v2Char(1:1) <= 'Z') then
         isBig(vCounter) = .true.
      end if
      call renameVertex(j, vCounter, v2char)
    end if
  end do
  print *, input
  allocate(adjMatrix(vCounter,vCounter))
  call populateMatrix()
  !do i=1,vcounter
  !  if(isBig(i) .eqv. .false.) then
  !    do j=1,vcounter
  !      adjMatrix(i, j) = 0
  !    end do
  !  end if
  !end do
  print *, "start = ", sIndex
  print *, "end = ", eIndex
  do i=1, vCounter
    print *, adjMatrix(i,:)
  end do
  print *, "visit"
  path = 0
  visited = .false.
  call visit(sIndex, 0, path)
  print *, totalPaths
contains
  recursive subroutine visit(x, length, path)
      integer, value :: x
      integer, value :: length
      integer :: path(8)
      integer :: mypath(8)
      integer :: i
      !if (length .gt. 8) then
      !        !print *, "Lenght greatern than 8 in", path
      !        return
      !end if
      if ((visited(x) .eqv. .true.) .and. (isBig(x) .eqv. .false.)) then
              !print *, "Already visited", x, " in ", path
              return
      end if
      !mypath = path
      !mypath(length+1) = x
      visited(x) = .true.
      print *, mypath 
      if (x .ne. eIndex) then
        do i=1,vCounter
          if (adjMatrix(x,i) .eq. 1) then
            !print *, "Next attepmt from ", x , " to ", i, " in ", mypath
            call visit(i, length+1, mypath)
            !print *, "Proceeding Next attepmt from ", x , " to ", i, " in ", mypath
          else
            !print *, "No vertex from ", x, "->", i
          end if
        end do
      else
        totalPaths = totalPaths + 1
      end if
      !mypath(length+1) = 0
      visited(x) = .false.
  end subroutine

  subroutine populateMatrix()
    integer :: tmp1
    integer :: tmp2
    adjMatrix = 0
    do i=1,numEdge
      call readEdge(input(i))
      read(v1char,*) tmp1
      read(v2char,*) tmp2
      adjMatrix(tmp1,tmp2) = 1
      adjMatrix(tmp2,tmp1) = 1
    end do
  end subroutine
  function notAnumber(ch)
     character(len=*) :: ch
     logical :: notAnumber
     if ((ch(1:1) >= 'a' .and. ch(1:1) <= 'z') .or. (ch(1:1) >= 'A' .and. ch(1:1) <= 'Z')) then
       notAnumber = .true.
     else
       notAnumber = .false.
     end if
  end function
  subroutine renameVertex(startIndex, val, vchar)
    integer :: startIndex, val
    character(len=*) :: vchar
    print *, trim(vchar), len_trim(vchar), val
    if(trim(vchar) == "start") then
       sIndex = val
    end if
    if(trim(vchar) == "end") then
       eIndex = val
    end if
    write(tmp1String,*) val
    do i=startIndex,numEdge
      if(input(i)(1:scan(input(i),'-')-1) == trim(vchar)) then
        input(i) = adjustl(trim(tmp1String)) // '-' // adjustl(trim(input(i)(scan(input(i),'-')+1:len(input(i)))))
      end if
      if(input(i)(scan(input(i),'-')+1:len(input(i))) == trim(vchar)) then
        input(i) = adjustl(trim(input(i)(1:scan(input(i),'-')-1))) // '-' // adjustl(trim(tmp1String))
      end if
    end do
  end subroutine
  subroutine readEdge(x)
    character(len=*) :: x
    print *, scan(x,'-')
    v1char = adjustl(trim(x(1:scan(x,'-')-1)))
    v2char = adjustl(trim(x(scan(x,'-')+1:len(x))))
    !print *, "readEdge", trim(v1char), "+", trim(v2char)
  end subroutine
end program mn
