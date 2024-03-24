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
        integer :: n_imgG_original, n_imgP_original 
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
        procedure :: get_img_g
        procedure :: get_img_p
        procedure :: get_size
        procedure :: get_name_cliente
        procedure :: get_num_ventanilla
        procedure :: get_total_pasos
        procedure :: verificar_completo
        procedure :: get_min_value
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


    function get_min_value(self) result(min_value)
        class(lista_circular), intent(in) :: self
        integer :: min_value
        type(node), pointer :: current
        min_value = 0 ! Valor inicial

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head
        min_value = current%value

        do
            current => current%next
            if (current%value < min_value) then
                min_value = current%value
            end if
            if (associated(current, self%head)) exit
        end do
    end function get_min_value


    function get_total_pasos(self, index) result(total_pasos)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        integer :: total_pasos
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                total_pasos = current%total_pasos
                return
            end if
            current => current%next
        end do

        ! print *, "El índice especificado no se encontró en la lista."
    end function get_total_pasos

    function get_name_cliente(self, index) result(name_cliente)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        character(len=:), allocatable :: name_cliente
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                name_cliente = current%name_cliente
                return
            end if
            current => current%next
        end do

        ! print *, "El índice especificado no se encontró en la lista."
    end function get_name_cliente

    function get_num_ventanilla(self, index) result(num_ventanilla)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        integer :: num_ventanilla
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                num_ventanilla = current%num_ventanilla
                return
            end if
            current => current%next
        end do

        ! print *, "El índice especificado no se encontró en la lista."
    end function get_num_ventanilla

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

    function get_img_p(self, index) result(img_g_value)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        integer :: img_g_value
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                img_g_value = current%n_imgP_original
                return
            end if
            current => current%next
        end do

        ! print *, "El índice especificado no se encontró en la lista."
    end function get_img_p

    function get_img_g(self, index) result(img_g_value)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        integer :: img_g_value
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                img_g_value = current%n_imgG_original
                return
            end if
            current => current%next
        end do

    end function get_img_g

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
        new%n_imgG_original= img_g
        new%n_imgP_original= img_p

        new_subnode%value = 'vacio' ! Inicializar la pila vacía

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

    function append_img(self) result(img_tip)
        class(lista_circular), intent(inout) :: self      
        type(node), pointer :: current
        integer :: img_tip

        current => self%head

        ! primero se mira si la lista no está vacía
            do while (associated(current))
                if (current%img_g > 0) then
                    current%img_g = current%img_g - 1
                    call append_to_stack(current, 'Imagen Grande')
                    img_tip= return_tipo_img("Imagen Grande")
                    print *, "Se entrego una Imagen Grande al cliente ", current%name_cliente
                    return
                
                else if (current%img_p > 0) then
                    current%img_p = current%img_p - 1
                    call append_to_stack(current, 'Imagen Pequena')
                    img_tip= return_tipo_img("Imagen Pequena")
                    print *, "Se entrego una Imagen Pequena al cliente", current%name_cliente
                    return
                end if
                ! Para evitar un bucle infinito
                if (associated(current%next, self%head)) then
                    ! print *, "la cabeza es:", self%head%value
                    exit
                end if
                current => current%next
            end do

        end function append_img

    function return_tipo_img(tipo) result(img_value)
        character(len=*), intent(in) :: tipo
        integer :: img_value

        select case (trim(adjustl(tipo)))
            case ("Imagen Grande")
                img_value = 1 ! O el valor que corresponda para "IMG grande"
            case ("Imagen Pequena")
                img_value = 2 ! O el valor que corresponda para "IMG pequeña"
            case default
                ! print *, "Tipo de imagen no válido."
                img_value = -1 ! O cualquier valor que indique que el tipo de imagen no es válido
        end select
    end function return_tipo_img


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
            print *, 'Numero Ventanilla:', current%num_ventanilla
            print *, 'Imagen G:', current%img_g
            print *, 'Imagen P:', current%img_p
            print *, 'Total Pasos:', current%total_pasos

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
        write(unit, *) 'digraph lisat_espera {'
        write(unit, *) 'label= "Lista de espera";'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];'

        current => self%head
        count = 0
        count_pila = 0

        do while (associated(current))
            count = count + 1
            first_img = .true.

            img= current%n_imgG_original
            imp= current%n_imgP_original

            write(unit, *) '    "Node', current%value, '" [label="', current%name_cliente,"\n Img_g =",img,"\n Img_p =",imp,'"];'

            !union de nodos

            write(unit, *) '    "Node', current%value, '" -> "Node', current%next%value, '";'
            write(unit, *) '    "Node', current%value, '" -> "Node', current%prev%value, '";'



            ! Graficar la pila
            stack=> current%stack
            do while (associated(stack))
                count_pila = count_pila + 1
                    
                if (.not.(stack%value=="vacio")) then
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

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpdf ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.pdf')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.pdf'

    end subroutine graficar

    function verificar_completo(self, index) result(complete)
        class(lista_circular), intent(inout) :: self
        integer, intent(in) :: index
        logical :: complete
        type(node), pointer :: current
        complete = .false.

        current => self%head

        do while (associated(current))
            if (current%value == index .and. current%img_g == 0 .and. current%img_p == 0) then
                complete = .true.
                return
            end if
            current => current%next
            if (associated(current, self%head)) exit  ! Salir al completar un ciclo (lista circular)
        end do

        complete = .false.
    end function verificar_completo


end module lista_clientes_espera

! program main
!     use lista_clientes_espera
!     implicit none

!     type(lista_circular) :: list

!     integer :: min


!     ! Agregar nodos a la lista
!     call list%append(1, "Cliente1", 1, 1, 1, 6)
!     call list%append(10, "Cliente2", 2, 3, 3, 7)
!     ! call list%append(3, "Cliente3", 3, 4, 2, 8)

!     ! call list%append_img(2, 'valor3') ! Agrega el valor 'valor3' a la pila del nodo con índice 2
!     ! call list%append_img(2, 'valor4') ! Agrega el valor 'valor4' a la pila del nodo con índice 3


!     ! Imprimir la lista
!     ! call list%print()

!     ! size_list= list%get_size()
!     ! print *, "El tamaño de la lista es: ", size_list


!     ! Eliminar un nodo de la lista
!     print *, "-------------------------------"
!     call list%delete(1)
!     ! call list%print()
!     call list%append(3, "Cliente3", 3, 4, 2, 8)
!     call list%append(4, "Cliente4", 4, 5, 1, 9)

!     ! ! Imprimir la lista después de la eliminación
!     call list%print()

!     ! Obtener el valor más pequeño
!     min= list%get_min_value()
!     print *, "El valor más pequeño es: ", min

! end program main
