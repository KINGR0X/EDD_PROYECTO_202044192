module lista_clientes_espera
    implicit none
    private

    type :: sub_node
        character(:), allocatable :: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type, public :: node
        private
        integer :: value
        character(:), allocatable :: name_cliente
        integer :: img_g, img_p, total_pasos, num_ventanilla
        type(sub_node), pointer :: stack => null() ! Pila asociada al nodo
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
    
    contains
        procedure :: append_to_stack
        
    end type node

    ! Lista circular
    type, public :: lista_circular
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: push_to_stack
    end type lista_circular

contains

    subroutine append(self, value, name_cliente, num_ventanilla, img_g, img_p, total_pasos)
        class(lista_circular), intent(inout) :: self
        integer, intent(in) :: value
        character(len=*), intent(in) :: name_cliente
        integer, intent(in) :: img_g, img_p, total_pasos, num_ventanilla

        type(node), pointer :: new
        type(sub_node), pointer :: new_subnode
        type(node), pointer :: aux

        allocate(new)
        allocate(new_subnode)

        new%value = value
        new%name_cliente = name_cliente
        new%num_ventanilla = num_ventanilla
        new%img_g = img_g
        new%img_p = img_p
        new%total_pasos = total_pasos

        new_subnode%value = '' ! Inicializar la pila vacía

        new%stack => new_subnode

        if (associated(self%head)) then
            aux => self%head
            do while(.not. associated(aux%next, self%head))
                aux => aux%next
            end do
            new%prev => aux ! asignar el nodo anterior
            new%next => self%head
            aux%next => new
            self%head%prev => new ! ajustar el prev del primer nodo para que apunte al nuevo nodo
        else    
            new%next => new
            self%head => new
            new%prev => new ! el único nodo en la lista, su prev es él mismo
        end if

        self%tail => new ! asignar la cola de la lista al nuevo nodo agregado
    end subroutine append


    subroutine print(self)
        class(lista_circular), intent(inout) :: self
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            print *, 'Index nodo:', current%value
            print *, 'Name Cliente:', current%name_cliente
            print *, 'Numero Ventanilla:', current%num_ventanilla
            print *, 'Imagen G:', current%img_g
            print *, 'Imagen P:', current%img_p
            print *, 'Total Pasos:', current%total_pasos
            call print_stack(current%stack)
            print *, ""
            if (associated(current%next, self%head)) exit  ! Salir al completar un ciclo (lista circular)
            current => current%next       
        end do
    end subroutine

    subroutine print_stack(stack)
        type(sub_node), pointer :: stack
        print *, 'Stack:'
        do while (associated(stack))
            print *, stack%value
            stack => stack%next
        end do
    end subroutine print_stack

    subroutine delete(self, value)
        class(lista_circular), intent(inout) :: self
        integer, intent(in) :: value
        type(node), pointer :: current, previous, new_tail

        if (.not. associated(self%head)) then
            print *, "La lista está vacía. No se puede eliminar el valor: ", value
            return
        end if

        current => self%head
        previous => null()

        ! Buscar el nodo con el valor dado
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encuentra el nodo, eliminarlo
        if (associated(current) .and. current%value == value) then
            if (associated(previous)) then
                ! El nodo a eliminar no es el primero
                previous%next => current%next
                current%next%prev => previous ! Actualizar prev del nodo siguiente al nodo a eliminar
            else
                ! El nodo a eliminar es el primero
                self%head => current%next
                current%next%prev => null() ! El nuevo primer nodo no tiene previo
            end if

            if (associated(current,self%tail)) then
                if (associated(previous)) then
                    new_tail => previous ! Si el nodo eliminado es la cola, entonces el nuevo nodo de cola es el anterior
                else
                    new_tail => null() ! Si el nodo eliminado es el único, entonces la cola ahora es null
                end if
            else
                new_tail => self%tail
            end if
            
            deallocate(current)
            self%tail => new_tail ! Actualizar la cola de la lista
            print *, "Se ha eliminado correctamente el valor: ", value
        else
            print *, "No se ha encontrado el valor: ", value
        end if
    end subroutine delete

    subroutine append_to_stack(this, value)
        class(node), intent(inout) :: this
        character(len=*), intent(in) :: value
        
        type(sub_node), pointer :: new_subnode, temp
        allocate(new_subnode)
        new_subnode%value = value

        if (.not. associated(this%stack)) then
            this%stack => new_subnode
        else
            temp => this%stack
            do while (associated(temp%next))
                temp => temp%next
            end do
            temp%next => new_subnode
        end if
    end subroutine append_to_stack

    subroutine push_to_stack(self, index, value)
        class(lista_circular), intent(inout) :: self
        integer, intent(in) :: index
        character(len=*), intent(in) :: value
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                call append_to_stack(current, value)
                return
            end if
            current => current%next
        end do

        print *, "El índice especificado no se encontró en la lista."
    end subroutine push_to_stack

end module lista_clientes_espera

! program main
!     use lista_clientes_espera
!     implicit none
    
!     type(lista_circular) :: list

!     ! Agregar nodos a la lista
!     call list%append(1, "Cliente1", "Ventanilla1", 2, 1, 6)
!     call list%append(2, "Cliente2", "Ventanilla2", 3, 3, 7)
!     call list%append(3, "Cliente3", "Ventanilla3", 4, 2, 8)

!     call list%push_to_stack(1, 'valor1') ! Agrega el valor 'valor1' a la pila del nodo con índice 1
!     call list%push_to_stack(2, 'valor2') ! Agrega el valor 'valor2' a la pila del nodo con índice 2
!     call list%push_to_stack(2, 'valor3') ! Agrega el valor 'valor3' a la pila del nodo con índice 2
!     call list%push_to_stack(2, 'valor4') ! Agrega el valor 'valor4' a la pila del nodo con índice 3


!     ! Imprimir la lista
!     call list%print()



!     ! Eliminar un nodo de la lista
!     print *, " "
!     ! call list%delete(2)

!     ! ! Imprimir la lista después de la eliminación
!     ! call list%print()
! end program main
