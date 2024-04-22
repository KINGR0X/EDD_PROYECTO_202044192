module avl_m
    use abb_m
    use uuid_module
    implicit none
    private

    type :: node
        integer, allocatable :: capas(:)
        integer :: value
        integer :: height = 1
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
        type(abb), pointer :: abb_tree => null()
        

    end type node

    type, public :: avl
        type(node), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: graph
        procedure :: insertAtNodeValue
    end type avl

contains

    !Subrutinas del tipo avl
    ! subroutine insert(self, val)
    !     class(avl), intent(inout) :: self
    !     integer, intent(in) :: val

    !     call insertRec(self%root, val)
    ! end subroutine insert

    subroutine insert(self, val, capas)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val
        integer, intent(in) :: capas(:)

        call insertRec(self%root, val, capas)
    end subroutine insert

    subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%root => deleteRec(self%root, val)
    end subroutine

    subroutine preorder(self)
        class(avl), intent(in) :: self
        
        call preorderRec(self%root)
    end subroutine preorder

    subroutine graph(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: command

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        command = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"


        write(io, *) "digraph G {"
        if(associated(self%root)) then
            call printRec(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)
        
        call execute_command_line(command, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if

    end subroutine graph

    !Subrutinas de apoyo
    recursive subroutine insertRec(root, val, capas)
        type(node), pointer, intent(inout) :: root
        integer, intent(in) :: val
        integer, intent(in) :: capas(:)

        if(.not. associated(root)) then
            allocate(root)
            root = node(value=val, capas=capas)
            !allocate(root%abb_tree) !Creo el arbol binario de busqueda
        else if(val < root%value) then
            call insertRec(root%left, val, capas)
        else if(val > root%value) then
            call insertRec(root%right, val, capas)
        end if

        root%height = maxHeight(getHeight(root%left), getHeight(root%right)) + 1

        if(getBalance(root) > 1) then
            if(getBalance(root%right) < 0) then
                root%right => rightRotation(root%right)
                root => leftRotation(root)
            else
                root => leftRotation(root)
            end if
        end if

        if(getBalance(root) < -1) then
            if(getBalance(root%left) > 0) then
                root%left => leftRotation(root%left)
                root => rightRotation(root)
            else
                root => rightRotation(root)
            end if
        end if
    end subroutine insertRec

    recursive function deleteRec(root, val) result(res)
        type(node), pointer :: root
        integer, intent(in) :: val
        type(node), pointer :: res
        type(node), pointer :: temp

        if(.not. associated(root)) then
            res => root
            return
        end if 

        if(val < root%value) then
            root%left => deleteRec(root%left, val)

        else if(val > root%value) then
            root%right => deleteRec(root%right, val)
        
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
        if(.not. associated(root)) return

        root%height = maxHeight(getHeight(root%left), getHeight(root%right)) + 1

        if(getBalance(root) > 1) then
            if(getBalance(root%right) < 0) then
                root%right => rightRotation(root%right)
                root => leftRotation(root)

            else
                root => leftRotation(root)
            end if
        end if

        if(getBalance(root) < -1) then
            if(getBalance(root%left) > 0) then
                root%left => leftRotation(root%left)
                root => rightRotation(root)

            else
                root => rightRotation(root)
            end if
        end if

        res => root
        
    end function deleteRec

    function leftRotation(root) result(rootRight)
        type(node), pointer, intent(in) :: root
        type(node), pointer :: rootRight
        type(node), pointer :: temp  
        
        rootRight => root%right
        temp => root%right%left

        rootRight%left => root
        root%right => temp

        root%height = maxHeight(getHeight(root%left), getHeight(root%right)) + 1
        rootRight%height = maxHeight(getHeight(rootRight%left), getHeight(rootRight%right)) + 1
    end function leftRotation

    function rightRotation(root) result(rootLeft)
        type(node), pointer, intent(in) :: root
        type(node), pointer :: rootLeft
        type(node), pointer :: temp

        rootLeft => root%left
        temp => rootLeft%right

        rootLeft%right => root
        root%left => temp

        root%height = maxHeight(getHeight(root%left), getHeight(root%right) + 1)
        rootLeft%height = maxHeight(getHeight(rootLeft%left), getHeight(rootLeft%right) + 1)
    end function rightRotation

    function maxHeight(left, right) result(res)
        integer, intent(in) :: left
        integer, intent(in) :: right

        integer :: res
        res = right

        if(left >= right) then
            res = left
            return
        end if
    end function maxHeight

    function getHeight(n) result(res)
        type(node), pointer, intent(in) :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%height
    end function getHeight

    function getBalance(root) result(res)
        type(node), intent(in) :: root

        integer :: res
        res = getHeight(root%right) - getHeight(root%left)
    end function getBalance

    recursive subroutine getMajorOfMinors(root, major)
        type(node), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    recursive subroutine preorderRec(root)
        type(node), pointer, intent(in) :: root

        if(associated(root)) then
            print *, root%value
            print *, root%capas
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    recursive subroutine printRec(root, name, io)
        type(node), pointer :: root
        character(len=36) :: name
        integer :: io

        character(len=36) :: right
        character(len=36) :: left

        right = generate_uuid()
        left = generate_uuid()

        if(associated(root)) then
            write(io, *)'"Nodo'//name//'"[label = "', root%value, '"]'
            if(associated(root%left)) then
                write(io, *)'"Nodo'//name//'"->"Nodo'//left//'"'
            end if
            if(associated(root%right)) then
                write(io, *)'"Nodo'//name//'"->"Nodo'//right//'"'
            end if
            call printRec(root%left, left, io)
            call printRec(root%right, right, io)

            ! if (associated(root%abb_tree)) then
            !     call printRec_abb(root%abb_tree%root, generate_uuid(), io)
            ! end if
        end if
    end subroutine printRec


    recursive subroutine insertAtNodeValue(self, val_avl, val_abb)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val_avl
        integer, intent(in) :: val_abb

        type(node), pointer :: found_node
        found_node => findNodeByValue(self%root, val_avl)

        if (associated(found_node)) then
            call found_node%abb_tree%insert(val_abb)
        else
            print *, "Error: No se encontró un nodo AVL con el valor especificado."
        end if
    end subroutine insertAtNodeValue

    recursive function findNodeByValue(node_avl, val) result(found_node)
        type(node), pointer :: node_avl
        integer, intent(in) :: val
        type(node), pointer :: found_node

        if (.not. associated(node_avl)) then
            found_node => null()
            return
        end if

        if (node_avl%value == val) then
            found_node => node_avl
            return
        elseif (val < node_avl%value) then
            found_node => findNodeByValue(node_avl%left, val)
        else
            found_node => findNodeByValue(node_avl%right, val)
        end if
    end function findNodeByValue


end module avl_m

! program main
!     use avl_m
!     implicit none
    
!     type(avl) :: tree

!     call tree%insert(0,[1,2,3])
!     call tree%insert(15,[10,9,8])
!     ! call tree%insert(30)
!     ! call tree%insert(48)
!     ! call tree%insert(26)
!     ! call tree%insert(10)
!     ! call tree%insert(7)
!     ! call tree%insert(5)
!     ! call tree%insert(60)
!     ! call tree%insert(19)
!     ! call tree%insert(11)
!     ! call tree%insert(21)
!     ! call tree%insert(3)

!     ! call tree%delete(11)
!     ! call tree%delete(10)
!     ! call tree%delete(19)

!     ! call tree%insertAtNodeValue(20, 2)
!     ! call tree%insertAtNodeValue(20, 5)

!     call tree%preorder()
!     call tree%graph()
! end program main