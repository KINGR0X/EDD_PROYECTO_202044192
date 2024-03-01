module lista_clientes_atentidos
    implicit none
    private

    type, public ::node
        private
        integer :: value
        character(:), allocatable :: name_cliente
        integer :: img_g, img_p, total_pasos, num_ventanilla
        type(node), pointer :: next     
    end type node

    type, public :: lista_simple_atendidos
        private
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: search
        procedure :: print_pointers
    end type lista_simple_atendidos

contains

    subroutine push(this, value)
        class(lista_simple_atendidos), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        ! print *, 'pushed ', value
    end subroutine push

    subroutine append(this,value, name_cliente, num_ventanilla, img_g, img_p, total_pasos)
        class(lista_simple_atendidos), intent(inout) :: this
        integer, intent(in) :: value
        character(len=*), intent(in) :: name_cliente
        integer, intent(in) :: img_g, img_p, total_pasos, num_ventanilla

        type(node), pointer :: temp
        type(node), pointer :: current

        allocate(temp)
        temp%value = value
        temp%name_cliente = name_cliente
        temp%num_ventanilla = num_ventanilla
        temp%img_g = img_g
        temp%img_p = img_p
        temp%total_pasos = total_pasos
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if

        ! print *, 'appended ', value
    end subroutine append

    subroutine delete(this, value)
        class(lista_simple_atendidos), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            ! print *, 'Se eliminó el valor ', value
        else
            ! print *, 'No se encontró el valor ', value
        end if

    end subroutine delete

    function search(this, value) result(retval)
        class(lista_simple_atendidos), intent(in) :: this
        integer, intent(in) :: value

        type(node), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(lista_simple_atendidos), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, 'Index nodo:', current%value
            print *, 'Name Cliente:', current%name_cliente
            print *, 'Numero Ventanilla:', current%num_ventanilla
            print *, 'Imagen G:', current%img_g
            print *, 'Imagen P:', current%img_p
            print *, 'Total Pasos:', current%total_pasos
            print *, '-----------------'
            current => current%next
        end do 
    end subroutine print

    subroutine print_pointers(this)
        class(lista_simple_atendidos), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, 'Nodo actual: ', current%value, ', Nodo siguiente: ', current%next%value
            current => current%next
        end do 
    end subroutine print_pointers
    
end module lista_clientes_atentidos

! program main
!     use lista_clientes_atentidos
!     implicit none

!     type(lista_simple_atendidos) :: list

!     call list%append(1, "Cliente1", 1, 1, 1, 6)
!     call list%append(2, "Cliente2", 2, 2, 2, 5)
!     call list%append(3, "Cliente3", 3, 3, 3, 4)
!     ! call list%push(10)
!     ! call list%push(20)

!     print *, '//-----------------//'
!     print *, 'La lista es:'
!     print *, '//-----------------//'
!     call list%print()
!     call list%print_pointers()

!     ! print *, '//-----------------//'
!     ! if(list%search(1)) then
!     !     print *, 'El valor 1 esta en la lista'
!     ! else
!     !     print *, 'El valor 1 no esta en la lista'
!     ! end if
!     ! call list%delete(1)
!     ! if(list%search(1)) then
!     !     print *, 'El valor 1 esta en la lista'
!     ! else
!     !     print *, 'El valor 1 no esta en la lista'
!     ! end if

!     ! print *, '//-----------------//'
!     ! print *, 'La lista es:'
!     ! print *, '//-----------------//'
!     ! call list%print()

! end program main