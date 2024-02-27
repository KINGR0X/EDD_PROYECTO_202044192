module cola_recepcion
    implicit none
    private

    type, public :: node
        private
        character(:), allocatable :: value
        integer :: img_g, img_p
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
        procedure :: return_cliente_name
        procedure :: return_cliente_img_g
        procedure :: return_cliente_img_p
    end type cola

contains

    subroutine append(this, value, img_g, img_p)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: value
        integer, intent(in) :: img_g, img_p

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
            print *, 'Cola recepcion esta vacia'
            return
        end if

        print *, 'Cliente eliminado de cola de recepcion: ', this%head%value
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

    function return_cliente_name(this) result(head_value)
        class(cola), intent(in) :: this
        character(len=:), allocatable :: head_value

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            head_value = ""
        else
            head_value = this%head%value
        end if
    end function return_cliente_name

    function return_cliente_img_g(this) result(img_g)
        class(cola), intent(in) :: this
        integer :: img_g

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            img_g = 0
        else
            img_g = this%head%img_g
        end if
    end function return_cliente_img_g

    function return_cliente_img_p(this) result(img_p)
        class(cola), intent(in) :: this
        integer :: img_p

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            img_p = 0
        else
            img_p = this%head%img_p
        end if
    end function  return_cliente_img_p

end module cola_recepcion

! program main
!     use cola_recepcion
!     implicit none

!     type(cola) :: cola_r
!     character(len=:), allocatable :: nombre_cliente
    
!     call cola_r%append("Juan", 1, 2)

!     call cola_r%print()

!     nombre_cliente = cola_r%return_cliente_name()

!     print *, "Nombre del cliente: ", nombre_cliente


! end program main