module pila_module
    implicit none
    private

    type, public :: node
        private
        integer :: value
        type(node), pointer :: next     
    end type node

    type, public :: pila
        private
        type(node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: print
    end type pila

contains

    subroutine push(this, value)
        class(pila), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => this%top
        this%top => temp

        print *, 'Pushed ', value
    end subroutine push

    subroutine pop(this)
        class(pila), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%top)) then
            print *, 'Pila esta vacia.'
            return
        end if
        temp => this%top

        print *, 'Popped', temp%value

        this%top => this%top%next
        deallocate(temp)
    end subroutine pop

    subroutine print(this)
        class(pila), intent(in) :: this
        type(node), pointer :: current

        current => this%top

        print *, '//-----------------//'
        print *, 'La pila es:'
        print *, '//-----------------//'

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print
    
end module pila_module

! program main
!     use pila_module
!     implicit none

!     type(pila) :: p
!     integer :: i

!     do i = 1, 10
!         call p%push(i)
!     end do

!     call p%print

!     do i = 1, 5
!         call p%pop
!     end do

!     call p%print

! end program main
