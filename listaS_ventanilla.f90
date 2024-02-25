module List_of_list_vent
    implicit none
    private

    type :: sub_node
        character(:), allocatable :: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node
        integer :: index
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(sub_node), pointer :: stack => null() ! pila
        
    contains
        procedure :: append
        procedure :: push
        procedure :: pop
        procedure :: print
    end type node

    type, public :: List_of_list
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: insert
        procedure :: printList
        procedure :: printPunteros
        procedure :: popAtIndex
    end type List_of_list

contains

    subroutine insert(self, index, value)
        class(List_of_list), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer, intent(in) :: index

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            allocate(aux)
            aux%index = index
            self%head => aux
            self%tail => aux
            call aux%append(value)
        else
            if(index < self%head%index) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%index = index
                call new%append(value)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                            call aux%append(value)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%index = index
                            call new%append(value)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then
                    call aux%append(value)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%index = index
                    call new%append(value)
                end if
            end if
        end if

        ! null
        if(associated(new%next)) then
            new%next => null()
        end if
    end subroutine insert

    subroutine printList(self)
        class(List_of_list) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'Indice: ', aux%index
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    subroutine printPunteros(self)
        class(List_of_list) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'Indice: ', aux%index
            call aux%print()
            print *, "El nodo siguiente es:", aux%next%index
            print *, "El nodo anterior es:", aux%prev%index
            print *, ""
            aux => aux%next
        end do
    end subroutine printPunteros

    ! Subrutina y funciones de sub nodo
    subroutine append(self, string)
        class(node), intent(inout) :: self
        character(len=*), intent(in) :: string

        type(sub_node), pointer :: new
        allocate(new)
        new%value = string

        ! Agregar al principio de la pila
        new%next => self%stack
        self%stack => new
    end subroutine append

    subroutine push(this, value)
        class(node), intent(inout) :: this
        character(len=*), intent(in) :: value

        call this%append(value)
    end subroutine push

    subroutine pop(this)
        class(node), intent(inout) :: this

        type(sub_node), pointer :: temp

        if (.not. associated(this%stack)) then
            print *, 'La pila está vacía.'
            return
        end if

        temp => this%stack
        this%stack => this%stack%next

        print *, '******** Pop:', temp%value, " ********"

        deallocate(temp)
    end subroutine pop

    subroutine popAtIndex(self, index)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: index

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                call current%pop()
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end subroutine popAtIndex

    subroutine print(self)
        class(node), intent(inout) :: self

        type(sub_node), pointer :: current
        current => self%stack
        print *, '------------------------------'
        print *, 'Pila:'

        do while (associated(current))
            print *, current%value
            current => current%next
        end do
    end subroutine print
end module List_of_list_vent

! program main
!     use List_of_list_vent
!     implicit none

!     type(List_of_list) :: list
!     call list%insert(1, 'inicio')
!     call list%insert(1, 'adios')
!     call list%insert(2, 'hola2')
!     call list%insert(2, 'adios2')
!     call list%insert(4, 'hola4')
!     call list%insert(4, 'adios4')
!     call list%insert(3, 'hola3')

!     call list%insert(1,'bye')

!     call list%printList()

!     call list%printPunteros()
!     print *, ""

!     ! pop 
!     call list%popAtIndex(1)
!     call list%printList()


! end program main
