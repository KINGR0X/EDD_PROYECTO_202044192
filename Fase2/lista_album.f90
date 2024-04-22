module lista_album
    implicit none
    private

    type :: sub_node
        integer:: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type, public :: node
        private
        integer :: value
        character(:), allocatable :: name_cliente
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
        procedure :: append_img
        procedure :: get_size
        procedure :: graficar
        procedure :: get_stack_size
        procedure :: print_pointers
    end type lista_circular

contains

    subroutine print_pointers(self)
        class(lista_circular), intent(inout) :: self
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            print *, 'Index nodo:', current%value
            print *, 'Prev:', current%prev%value
            print *, 'Next:', current%next%value
            print *, ""

            if (associated(current%next, self%head)) exit  ! Salir al completar un ciclo (lista circular)
            current => current%next       
        end do
    end subroutine print_pointers

    function get_size(self) result(list_size)
        class(lista_circular), intent(inout) :: self
        type(node), pointer :: current
        integer :: list_size
        list_size = 0

        if (.not. associated(self%head)) then
            ! print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            list_size = list_size + 1
            if (associated(current%next, self%head)) exit
            current => current%next       
        end do
    end function get_size

    subroutine append(self,value, name_cliente)
        class(lista_circular), intent(inout) :: self
        integer, intent(in) :: value
        character(len=*), intent(in) :: name_cliente

        type(node), pointer :: new
        type(sub_node), pointer :: new_subnode
        type(node), pointer :: aux

        allocate(new)
        allocate(new_subnode)

        new%value = value
        new%name_cliente = name_cliente


        new_subnode%value = -1 ! Inicializar la pila vacía

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


    subroutine delete(self, value)
        class(lista_circular), intent(inout) :: self
        integer, intent(in) :: value
        type(node), pointer :: current, previous, new_tail

        if (.not. associated(self%head)) then
            print *, "La lista está vacía. No se puede eliminar el valor: ", value
            return
        end if

        current => self%head
        previous => self%tail ! Para mantener el puntero al nodo anterior al current
        
        ! Buscar el nodo con el valor dado
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
            if (associated(current, self%head)) exit ! Salir si se llega al final de la lista
        end do

        ! Si se encuentra el nodo, eliminarlo
        if (associated(current) .and. current%value == value) then
            if (associated(current, self%head)) then
                ! El nodo a eliminar es el primero
                if (associated(current%next, self%head)) then
                    ! Si la lista tiene solo un nodo
                    deallocate(current)
                    self%head => null()
                    self%tail => null()
                else
                    self%head => current%next
                    self%tail%next => current%next
                    current%next%prev => self%tail
                    deallocate(current)
                end if
            else if (associated(current, self%tail)) then
                ! El nodo a eliminar es el último
                previous%next => self%head
                self%tail => previous
                deallocate(current)
            else
                ! El nodo a eliminar está en el medio
                previous%next => current%next
                current%next%prev => previous
                deallocate(current)
            end if
            ! print *, "Se ha eliminado correctamente el valor: ", value
        else
            ! print *, "No se ha encontrado el valor: ", value
        end if
    end subroutine delete


    subroutine append_to_stack(this, value)
        class(node), intent(inout) :: this
        integer, intent(in) :: value
        
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


    subroutine append_img(self,album_name,id_img)
        class(lista_circular), intent(inout) :: self      
        type(node), pointer :: current
        character(len=*), intent(in) :: album_name
        integer, intent(in) :: id_img

        current => self%head

        ! primero se mira si la lista no está vacía
            do while (associated(current))
                if (current%name_cliente == album_name) then
                    call append_to_stack(current, id_img)
                    return
                
                end if
                ! Para evitar un bucle infinito
                if (associated(current%next, self%head)) then
                    ! print *, "la cabeza es:", self%head%value
                    exit
                end if
                current => current%next
            end do

    end subroutine append_img



    subroutine print(self)
        class(lista_circular), intent(inout) :: self
        type(node), pointer :: current
        type(sub_node), pointer :: stack

        if (.not. associated(self%head)) then
            ! print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            print *, 'Index nodo:', current%value
            print *, 'Name Cliente:', current%name_cliente

            ! Print del stack
            print *, 'Stack:'
            stack=> current%stack
            do while (associated(stack))
                print *, stack%value
                stack => stack%next
            end do

            print *, ""
            if (associated(current%next, self%head)) exit  ! Salir al completar un ciclo (lista circular)
            current => current%next       
        end do
    end subroutine print

    function get_stack_size(self, index) result(stack_size)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        type(node), pointer :: current
        type(sub_node), pointer :: stack
        integer :: stack_size

        stack_size = 0

        current => self%head

        do while (associated(current))
            if (current%value == index) then
                if (associated(current%stack)) then
                    stack => current%stack
                    do while (associated(stack))
                        stack_size = stack_size + 1
                        if (associated(stack%next)) then
                            stack => stack%next
                        else
                            exit
                        end if
                    end do
                end if
                exit
            end if
            if (associated(current%next, self%head)) exit
            current => current%next
        end do
    end function get_stack_size

    subroutine graficar(self,filename)
        class(lista_circular), intent(inout) :: self
        character(len=*), intent(in) :: filename
        type(node), pointer :: current
        type(sub_node), pointer :: stack
        logical :: first_img

        integer :: img, imp

        integer :: unit
        integer :: count, count_pila

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph albumes {'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];'

        current => self%head
        count = 0
        count_pila = 0

        do while (associated(current))
            count = count + 1
            first_img = .true.

            write(unit, *) '    "Node', current%value, '" [label="', current%name_cliente,'"];'

            !union de nodos

            write(unit, *) '    "Node', current%value, '" -> "Node', current%next%value, '";'
            write(unit, *) '    "Node', current%value, '" -> "Node', current%prev%value, '";'



            ! Graficar la pila
            stack=> current%stack
            do while (associated(stack))
                count_pila = count_pila + 1
                    
                if (.not.(stack%value==-1)) then
                    if (first_img) then
                        write(unit, *) '    "Node_img', count_pila, '" [label="',stack%value,'"];'
                        write(unit, *) '    "Node_img', count_pila, '" -> "Node', current%value, '";'
                        first_img = .false.
                    else
                        write(unit, *) '    "Node_img', count_pila, '" [label="',stack%value,'"];'
                        write(unit, *) '    "Node_img', count_pila, '" -> "Node_img', count_pila-1, '";'
                    end if

                end if

                stack => stack%next
            end do
                
            if (associated(current%next, self%head)) exit  ! Salir
            current => current%next     
        end do

        ! Se establecen que nodos conforman una fila
        write(unit, '(*(g0))') ' '
        write(unit, '(*(g0))', advance="no") '{rank=same;'

        current => self%head
        do while (associated(current))

            write(unit, *) '    "Node', current%value, '";'

            if (associated(current%next, self%head)) exit  ! Salir
            current => current%next     
        end do

        write(unit, '(*(g0))',advance="yes") '};'

        ! salto de linea para que este tan pegado
        write(unit, '(*(g0))') ' '


        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'

    end subroutine graficar


end module lista_album

! program main
!     use lista_album
!     implicit none

!     type(lista_circular) :: list

!     ! Agregar nodos a la lista
!     call list%append(1,"Album 1")
!     call list%append(2,"Album 2")
!     call list%append(3,"Album 3")

!     call list%append_img("Album 1",1)
!     call list%append_img("Album 1",3)
!     call list%append_img("Album 1",5)
!     call list%append_img("Album 2",2)
!     call list%append_img("Album 2",5)

!     call list%graficar('lista_album.dot')

! end program main
