module cola_recepcion
    implicit none
    private

    type, public :: node
        private
        character(:), allocatable :: value,img_g, img_p
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: end => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
    end type cola

contains

    subroutine append(this, value, img_g, img_p)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: value,img_g, img_p

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%img_g = img_g
        temp%img_p = img_p
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
        class(cola), intent(inout) :: this
        type(node), pointer :: temp

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
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '-------- Cola de Recepcion --------'
        print *,  " "

        do while (associated(current))
            print *, current%value
            print *, "img_g: ", current%img_g
            print *, "img_p: ", current%img_p
            print *, "---------------"
            current => current%next
        end do 
    end subroutine print

end module cola_recepcion