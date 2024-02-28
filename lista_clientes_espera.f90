module linked_list_module
    implicit none
    private

    !NODO
    type, public :: node
        private
        integer :: value
        character(:), allocatable :: name_cliente, name_ventanilla
        integer :: img_g, img_p, total_pasos
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
    end type node

    !LISTA CIRCULAR
    type, public :: linked_list
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
    end type linked_list

contains

    subroutine append(self, value,name_cliente,name_ventanilla,img_g,img_p,total_pasos)
        class(linked_list), intent(inout) :: self
        integer, intent(in) ::  value
        character(len=*), intent(in) :: name_cliente, name_ventanilla
        integer, intent(in) :: img_g, img_p, total_pasos
        
        type(node), pointer :: new
        type(node), pointer :: aux
        allocate(new)


        new = node(value=value, &
           name_cliente=name_cliente, &
           name_ventanilla=name_ventanilla, &
           img_g=img_g, &
           img_p=img_p, &
           total_pasos=total_pasos, &
           next=null(), &
           prev=null())

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
        ! print *, "Se ha insertado correctamente el valor: ", value
    end subroutine append


    subroutine print(self)
        class(linked_list), intent(inout) :: self
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            print *, current%value, ","
            print *, "El nodo siguiente es: ", current%next%value
            print *, "El nodo anterior es: ", current%prev%value
            if (associated(current%next, self%head)) exit  ! Salir al completar un ciclo (lista circular)
            
            current => current%next       
        end do
    end subroutine

    subroutine delete(self, value)
        class(linked_list), intent(inout) :: self
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
                    new_tail => null() ! Si el nodo eliminado es la única, entonces la cola ahora es null
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
end module linked_list_module

program main
    use linked_list_module
    implicit none
    
    type(linked_list) :: list

    call list%append(1,"name_cliente","name_ventanilla",1,2,6)
    call list%append(2,"name_cliente2","name_ventanilla",1,2,6)
    call list%append(8,"name_cliente3","name_ventanilla",1,2,6)
    call list%append(4,"name_cliente4","name_ventanilla",1,2,6)

    ! call list%append(1)
    ! call list%append(2)
    ! call list%append(3)
    ! call list%append(4)

    ! call list%print()
    print *, " "
    call list%delete(4)
    ! call list%delete(3)

    call list%print()
end program main