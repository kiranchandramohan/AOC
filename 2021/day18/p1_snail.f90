program mn
  implicit none
  type node
    integer :: depth = 0
    integer :: val = -1
    type(node), pointer :: parent => NULL()
    type(node), pointer :: left => NULL()
    type(node), pointer :: right => NULL()
  end type
  type nodeptr
    type(node), pointer :: ptr => NULL()
  end type
  type stack
    integer :: top = 0
    type(nodeptr) :: chars(100)
  end type

  integer, parameter :: MAXSIZE = 100
  integer :: fd, ios
  character(len=20) :: inputFile = "input.txt"
  character(len=20) :: countNumbersChar
  integer :: countNumbers
  character(len=MAXSIZE) :: num
  integer :: i, j, k
  type(node), pointer :: root1
  type(node), pointer :: root2
  type(nodeptr) :: roottmp1
  type(nodeptr) :: roottmp2

  call get_command_argument(1, inputFile)
  call get_command_argument(2, countNumbersChar)
  read(countNumbersChar,*) countNumbers
  print *, inputFile, countNumbers
  open(newunit=fd, file=inputFile, iostat=ios)
  read(fd,'(A)') num
  !print *, trim(num), " : "
  root1 => createTree(num)
  roottmp1%ptr => root1
  call callAction(root1)
  !print *, "After action"
  !call printTree(root1)
  !print *, ""
  do i=2, countNumbers
    read(fd,'(A)') num
    !print *, trim(num), " : "
    root2 => createTree(num)
    !print *, "New input"
    !call printTree(root2)
    call callAction(root2)
    !print *, "After action"
    !call printTree(root2)
  !print *, ""
    roottmp2%ptr => root2
    roottmp1 = createNodeLR(roottmp1, roottmp2)
    root1 => roottmp1%ptr
    !print *, "Before action, combined tree"
    !call printTree(root1)
    call callAction(root1)
  !print *, "After action, combined tree"
    !call printTree(root1)
  !print *, ""
  end do
  close(fd)
  print *, computeMagnitude(root1)
contains
  subroutine doAllExplodes(n)
    type(node), pointer :: n
    do while (explodeOrSplit1(n))
      !print *, "Another Explosion round"
      !call printTree(n)
    end do
  end subroutine
  subroutine callAction(n)
    type(node), pointer :: n
    call doAllExplodes(n)
    do while (explodeOrSplit2(n))
      !print *, "Another round of split"
      !call printTree(n)
    call doAllExplodes(n)
    end do
  end subroutine
  recursive function explodeOrSplit1(n) result(rval)
    type(node), pointer :: n
    logical :: rval
    if ((associated(n%left) .eqv. .false.) .and. (associated(n%right) .eqv. .false.)) then
        rval = .false.
    else if (n%depth .ge. 4) then
        !print *, "Found node to explode ", n%left%val, n%right%val
        call explodeNode(n)
        rval = .true.
    else
      rval = explodeOrSplit1(n%left)
      if (rval .eqv. .false.) then
        rval = explodeOrSplit1(n%right)
      end if
    end if
  end function
 recursive function explodeOrSplit2(n) result(rval)
    type(node), pointer :: n
    logical :: rval
    if ((associated(n%left) .eqv. .false.) .and. (associated(n%right) .eqv. .false.)) then
      if (n%val .gt. 9) then
        !print *, "Found node to split ", n%val
        call splitNode(n)
        rval = .true.
      else
        rval = .false.
      end if
    else
      rval = explodeOrSplit2(n%left)
      if (rval .eqv. .false.) then
        rval = explodeOrSplit2(n%right)
      end if
    end if
  end function
 

  recursive subroutine splitIfAny(n)
    type(node), pointer :: n
    if ((associated(n%left) .eqv. .false.) .and. (associated(n%right) .eqv. .false.)) then
      return
    else
      call splitIfAny(n%left)
      if (n%val .gt. 9) then
        !print *, "Found node to split ", n%val
        call splitNode(n)
      end if
      call splitIfAny(n%right)
    end if
  end subroutine
  recursive subroutine explodeIfAny(n)
    type(node), pointer :: n
    if ((associated(n%left) .eqv. .false.) .and. (associated(n%right) .eqv. .false.)) then
      return
    else if (n%depth .ge. 5) then
        call explodeNode(n)
    else
      call explodeIfAny(n%left)
      call explodeIfAny(n%right)
    end if
  end subroutine
  recursive subroutine printTree(n)
    type(node), pointer :: n
    if ((associated(n%left) .eqv. .false.) .and. (associated(n%right) .eqv. .false.)) then
      !write(*,'(A1)',advance='no') '('
      !write(*,'(I1)',advance='no') n%depth
      !write(*,'(A1)',advance='no') ')'
      write(*,'(I2)',advance='no') n%val
    else
      write(*,'(A1)',advance='no') '['
      !write(*,'(A1)',advance='no') '('
      !write(*,'(I1)',advance='no') n%depth
      !write(*,'(A1)',advance='no') ')'
      call printTree(n%left)
      write(*,'(A1)',advance='no') ','
      call printTree(n%right)
      write(*,'(A1)',advance='no') ']'
    endif
  end subroutine
  recursive subroutine incrementDepth(n)
    type(node), pointer :: n
    if (associated(n) .eqv. .false.) then
      return
    end if
    n%depth = n%depth+1
    call incrementDepth(n%left)
    call incrementDepth(n%right)
  end subroutine
  function createNodeLR(l,r)
    type(nodeptr) :: l, r, createNodeLR
    allocate(createNodeLR%ptr)
    createNodeLR%ptr%left => l%ptr
    createNodeLR%ptr%right => r%ptr
    l%ptr%parent => createNodeLR%ptr
    r%ptr%parent => createNodeLR%ptr
    createNodeLR%ptr%depth = 0
    call incrementDepth(l%ptr)
    call incrementDepth(r%ptr)
  end function
  function createNode(v)
    integer :: v
    type(nodeptr) :: createNode
    allocate(createNode%ptr)
    createNode%ptr%val = v
  end function
  function inorderPredecessor(n)
    type(node), pointer :: n
    type(node), pointer :: inorderPredecessor
    type(node), pointer :: cur
    cur => n
    do
      if (associated(cur%parent) .eqv. .true.) then
        if (associated(cur, cur%parent%left) .eqv. .true.) then
          cur => cur%parent
          cycle
        end if
      end if
      exit
    end do

    if (associated(cur%parent) .eqv. .false.) then
      inorderPredecessor => NULL()
      return
    else
      cur => cur%parent%left
    end if

    do while (associated(cur%right))
      cur => cur%right
    end do

    inorderPredecessor => cur
  end function
  function inorderSuccessor(n)
    type(node), pointer :: n
    type(node), pointer :: inorderSuccessor
    type(node), pointer :: cur
    cur => n
    do
      if (associated(cur%parent) .eqv. .true.) then
        if (associated(cur, cur%parent%right) .eqv. .true.) then
          cur => cur%parent
          cycle
        end if
      end if
      exit
    end do

    if (associated(cur%parent) .eqv. .false.) then
      inorderSuccessor => NULL()
      return
    else
      cur => cur%parent%right
    end if

    do while (associated(cur%left))
      cur => cur%left
    end do

    inorderSuccessor => cur
  end function
  subroutine explodeNode(n)
    type(node), pointer :: n
    type(node), pointer :: pred
    type(node), pointer :: succ
    pred => inorderPredecessor(n)
    succ => inorderSuccessor(n)

    !print *, "Found node for exploding at depth = ", n%depth, n%left%val, n%right%val
    if(associated(pred)) then
      !print *, "Pred found with val = ", pred%val
      pred%val = pred%val + n%left%val
      !print *, "Incrementing left to ", pred%val
    end if
    if(associated(succ)) then
      !print *, "Succ found with val = ", succ%val
      succ%val = succ%val + n%right%val
      !print *, "Incrementing right to ", succ%val
    end if

    n%val = 0
    deallocate(n%left)
    deallocate(n%right)
    n%left => NULL()
    n%right => NULL()
  end subroutine
  subroutine splitNode(n)
    type(node), pointer :: n
    type(node), pointer :: newL
    type(node), pointer :: newR

    if (associated(n%left) .neqv. .false. .or. associated(n%right) .neqv. .false.) then
      stop "Malformed node in splitNode"
    end if
    allocate(newL)
    allocate(newR)
    n%left => newL
    n%right => newR
    newL%val = FLOOR(FLOAT(n%val)/FLOAT(2))
    newR%val = CEILING(FLOAT(n%val)/FLOAT(2))
    n%val = -1
    !n%depth = n%depth+1
    newL%parent => n
    newR%parent => n
    newL%depth = n%depth+1
    newR%depth = n%depth+1
    !print *, "Finish splitting ", n%parent%depth, n%depth, newL%depth, newR%depth
  end subroutine
  function createTree(c)
    character(len=*) :: c
    type(node), pointer :: createTree
    type(stack) :: mystack
    type(node), pointer :: left, right
    integer :: i
    do i=1,len_trim(c)
      if (c(i:i) .eq. '[') then
      else if (c(i:i) .eq. ',') then
      else if (c(i:i) .eq. ']') then
        mystack%chars(mystack%top-1) = createNodeLR(mystack%chars(mystack%top-1), mystack%chars(mystack%top))
        mystack%top = mystack%top-1
      else if (c(i:i) >= '0' .and. c(i:i) <= '9') then
        mystack%top = mystack%top+1
        mystack%chars(mystack%top:mystack%top) = createNode(IACHAR(c(i:i)) -IACHAR('0'))
      else
        print *, "Unidentified =", c(i:i)
        stop "Unidentified char"
      end if
    end do
    if (mystack%top .ne. 1) then
      stop "Malformed tree"
    end if
    createTree => mystack%chars(mystack%top)%ptr
  end function
  recursive function computeMagnitude(n) result(comp)
    type(node), pointer :: n
    integer(kind=8) :: comp
    if (associated(n) .eqv. .false.) then
      comp = 0
    else if ((associated(n%left) .eqv. .false.) .and. (associated(n%right) .eqv. .false.)) then
      comp = n%val
    else 
      comp = 3 * computeMagnitude(n%left) + 2 * computeMagnitude(n%right)
    end if
  end function
end program
