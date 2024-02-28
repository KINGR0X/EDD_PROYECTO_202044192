module cola_impresion
    implicit none
    private

    type, public :: node_im
        private
        character(:), allocatable :: value
        type(node_im), pointer :: next     
    end type node_im

    type, public :: cola_im
        private
        type(node_im), pointer :: head => null()
        type(node_im), pointer :: end => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: size_cola
    end type cola_im

contains

    subroutine append(this, value)
        class(cola_im), intent(inout) :: this
        character(len=*), intent(in) :: value

        type(node_im), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%end => temp
        else
            this%end%next => temp
            this%end => temp
        end if

        ! print *, 'Append ', value
        ! print *, 'Append ', img_g
        ! print *, 'Append ', img_p
    end subroutine append

    subroutine delete(this)
        class(cola_im), intent(inout) :: this
        type(node_im), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            return
        end if

        print *, 'Delete ', this%head%value
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

    subroutine print(this)
        class(cola_im), intent(in) :: this
        type(node_im), pointer :: current

        current => this%head

        print *, '-------- Cola de impresion --------'

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print

    function size_cola(this) result(size)
        class(cola_im), intent(in) :: this
        type(node_im), pointer :: current
        integer :: size

        current => this%head
        size = 0

        do while (associated(current))
            size = size + 1
            current => current%next
        end do
    end function size_cola


end module cola_impresion

! program main
!     use cola_impresion
!     implicit none

!     type(cola_im) :: cola_r
    
!     call cola_r%append("Imagen Grande")
!     call cola_r%append("Imagen Grande2")
!     call cola_r%append("Imagen Grande3")

!     call cola_r%print()
!     print *, 'Tamaño de la cola:', cola_r%size_cola()

!     call cola_r%delete()
!     call cola_r%delete()
!     call cola_r%print()
!     print *, 'Tamaño de la cola:', cola_r%size_cola()

! end program main