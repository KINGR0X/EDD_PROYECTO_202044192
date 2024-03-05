module List_of_list_vent
    implicit none
    private

    type :: sub_node
        character(:), allocatable :: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node
        integer :: index
        integer :: num_imgG=0
        integer :: num_imgP=0
        integer :: num_imgG_original=0
        integer :: num_imgP_original=0
        character(:), allocatable :: name_client
        logical :: ocupado = .false.
        type(node), pointer :: next => null()
        type(sub_node), pointer :: stack => null() ! pila
        
    contains
        procedure :: append
        procedure :: push
        procedure :: pop
        procedure :: print
        procedure :: pop_normal
    end type node

    type, public :: List_of_list
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: insert
        procedure :: printList
        procedure :: printPunteros
        procedure :: popAtIndex
        procedure :: asignar_datos_cliente
        procedure :: buscar_nodo_desocupado
        procedure :: imagenes_a_ventanilla
        procedure :: check_stack_size
        procedure :: verificar_nodo
        procedure :: pop_inicial
        procedure :: return_nombre_cliente
        procedure :: return_num_ventanilla
        procedure :: return_num_imgG
        procedure :: return_num_imgP
        procedure :: graficar 
    end type List_of_list

contains

    function return_nombre_cliente(self, index) result(name_client)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        character(len=:), allocatable :: name_client

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                name_client = current%name_client
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end function return_nombre_cliente

    function return_num_ventanilla(self, index) result(num_ventanilla)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        integer :: num_ventanilla

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                num_ventanilla = current%index
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end function return_num_ventanilla

    function return_num_imgG(self, index) result(num_imgG_original)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        integer :: num_imgG_original

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                num_imgG_original = current%num_imgG_original
                return
            end if
            current => current%next
        end do

        ! print *, 'El índice especificado no se encontró en la lista.'
    end function return_num_imgG

    function return_num_imgP(self, index) result(num_imgP_original)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        integer :: num_imgP_original

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                num_imgP_original = current%num_imgP_original
                return
            end if
            current => current%next
        end do

        ! print *, 'El índice especificado no se encontró en la lista.'
    end function return_num_imgP

    function verificar_nodo(self, index) result(pila_llena)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: index
        logical :: pila_llena

        type(node), pointer :: current
        current => self%head

        pila_llena = .false.

        do while (associated(current))
            if (current%index == index) then
                if (current%ocupado) then
                    ! para que espere un paso antes de pasar la lista de espera
                    if (current%num_imgG == 0 .and. current%num_imgP == 0) then
                        if (self%check_stack_size(index)>0) then
                            current%ocupado = .false.
                            pila_llena = .true.
                            ! print *, 'El nodo ', index, ' tiene la pila llena.'
                            ! print *, 'Ocupado:', current%ocupado
                        end if
                    else
                        ! print *, 'El nodo ', index, ' tiene imágenes por agregar'
                        ! print *, 'Número de imágenes grandes: ', current%num_imgG
                        ! print *, 'Número de imágenes pequeñas: ', current%num_imgP
                        pila_llena = .false.
                    end if
                end if
                exit
            end if
            current => current%next
        end do
    end function verificar_nodo


    function check_stack_size(self, index) result(size_p)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        integer :: stack_size
        integer :: size_p
        type(sub_node), pointer :: temp

        type(node), pointer :: current
        current => self%head

        size_p = 0

        do while (associated(current))
            if (current%index == index) then
                if (associated(current%stack)) then
                    ! Calcular el tamaño de la pila
                    temp => current%stack
                    stack_size = 0
                    do while (associated(temp))
                        stack_size = stack_size + 1
                        temp => temp%next
                    end do

                    ! Verificar si el tamaño de la pila es mayor a 0
                    if (stack_size > 0) then
                        size_p= stack_size
                    end if
                end if
                exit
            end if
            current => current%next
        end do
    end function check_stack_size

    subroutine imagenes_a_ventanilla(self)
        class(List_of_list), intent(inout) :: self

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%ocupado .and. current%num_imgG > 0) then
                ! Agregar una imagen grande
                call self%insert(current%index, 'Imagen Grande')
                print '(A,I2,A)', "La ventanilla ", current%index, " recibio una Imagen Grande."
                ! Decrementar el contador de imágenes grandes
                current%num_imgG = current%num_imgG - 1

            else if (current%ocupado .and. current%num_imgP > 0) then
                ! Agregar una imagen grande
                call self%insert(current%index, 'Imagen Pequena')
                print '(A,I2,A)', "La ventanilla ", current%index, " recibio una Imagen Pequena."
                ! Decrementar el contador de imágenes grandes
                current%num_imgP = current%num_imgP - 1
            end if
            current => current%next
        end do

    end subroutine imagenes_a_ventanilla

    subroutine insert(self, index, value)
        class(List_of_list), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer, intent(in) :: index

        type(node), pointer :: aux
        type(sub_node), pointer :: new_subnode
        allocate(new_subnode)
        new_subnode%value = value

        if (.not. associated(self%head)) then
            allocate(aux)
            aux%index = index
            self%head => aux
            self%tail => aux
            aux%stack => new_subnode
        else
            aux => self%head
            do while (associated(aux%next))
                if (index == aux%index) then
                    call aux%push(value)
                    return
                end if
                aux => aux%next
            end do

            if (index == aux%index) then
                call aux%push(value)
                return
            end if

            allocate(aux%next)
            aux%next%index = index
            aux%next%stack => new_subnode
            self%tail => aux%next
        end if
    end subroutine insert

    subroutine printList(self)
        class(List_of_list) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'Indice: ', aux%index
            print *, 'Nombre del cliente: ', aux%name_client
            print *, 'Numero de imagenes grandes: ', aux%num_imgG
            print *, 'Numero de imagenes pequeñas: ', aux%num_imgP
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    subroutine graficar(this,filename)
        class(List_of_list) ::this        
        character(len=*), intent(in) :: filename
        character(len=100) :: character_sin
        type(node), pointer :: aux
        type(sub_node), pointer :: current
        logical :: first_img

        integer :: img, imp

        integer :: unit
        integer :: count, count_pila

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph lista_ventanillas {'
        write(unit, *) 'label= "Lista ventanillas";'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];' ! Aplicar atributos a todos los nodos
        
        ! Escribir nodos y conexiones
        aux => this%head
        count = 0
        count_pila = 0
        

        do while(associated(aux))
            count = count + 1
            first_img = .true.

            write(unit, *) '    "Node', count, '" [label="', "Ventanilla ",aux%index,'"];'

            if (len_trim(trim(adjustl(aux%name_client))) > 0 ) then
                img= aux%num_imgG_original
                imp= aux%num_imgP_original

                character_sin = quitar_espacios_en_medio(aux%name_client)

                write(unit, *) 'subgraph cluster_c',character_sin,  "{"
                write(unit, *) '    "Node_c', count, '" [label="', aux%name_client,"\n Img_g =",img,"\n Img_p =",imp,'"];'
                write(unit, *) '    "Node_c', count, '" -> "Node', count, '";'
                write(unit, *) '}'

            end if

            if (associated(aux%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if

   
            ! Graficar la pila
            if (this%check_stack_size(count) > 0) then

                ! Imprimir información de la pila asociada al nodo
                current => aux%stack
                do while (associated(current))
                    count_pila = count_pila + 1

                    if (first_img) then
                        write(unit, *) '    "Node_img', count_pila, '" [label="',current%value,'"];'
                        write(unit, *) '    "Node_img', count_pila, '" -> "Node', count, '";'
                        first_img = .false.
                    else
                        write(unit, *) '    "Node_img', count_pila, '" [label="',current%value,'"];'
                        write(unit, *) '    "Node_img', count_pila, '" -> "Node_img', count_pila-1, '";'
                    end if

                    !se mira si es la primera imagen de la pila
                    !write(unit, *) '    "Node_img', count_pila, '" [label="',current%value,'"];'
                    !write(unit, *) '    "Node_img', count_pila, '" -> "Node', count, '";'

                    !write(unit, *) '    "Node_img', count_pila, '" [label="',current%value,'"];'
                    !write(unit, *) '    "Node_img', count_pila, '" -> "Node', count, '";'

                    current => current%next
                end do
            end if

            aux => aux%next
        end do

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpdf ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.pdf')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.pdf'

    end subroutine graficar

    function quitar_espacios_en_medio(cadena_original) result(cadena_sin_espacios)
        implicit none
        character(len=*) :: cadena_original
        character(len=100) :: cadena_sin_espacios
        integer :: i, longitud

        ! Obtener la longitud de la cadena original
        longitud = len_trim(cadena_original)

        ! Inicializar la cadena sin espacios
        cadena_sin_espacios = ""

        ! Iterar sobre la cadena y agregar caracteres no espaciales a la nueva cadena
        do i = 1, longitud
            if (cadena_original(i:i) /= ' ') then
                cadena_sin_espacios = TRIM(cadena_sin_espacios) // cadena_original(i:i)
            end if
        end do

    end function quitar_espacios_en_medio

    function buscar_nodo_desocupado(self) result(index)
        class(List_of_list), intent(in) :: self
        integer :: index
        
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (.not. current%ocupado) then
                index = current%index
                return
            end if
            current => current%next
        end do

        ! Si no se encuentra ningún nodo desocupado, devolvemos -1
        index = -1
    end function buscar_nodo_desocupado

    subroutine printPunteros(self)
        class(List_of_list) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'Indice: ', aux%index
            call aux%print()
            print *, "El nodo siguiente es:", aux%next%index
            print *, ""
            aux => aux%next
        end do
    end subroutine printPunteros

    ! Subrutina y funciones de sub nodo
    subroutine append(self, string)
        class(node), intent(inout) :: self
        character(len=*), intent(in) :: string

        type(sub_node), pointer :: new_subnode
        allocate(new_subnode)
        new_subnode%value = string

        ! Agregar al principio de la pila
        new_subnode%next => self%stack
        self%stack => new_subnode
    end subroutine append

    subroutine push(this, value)
        class(node), intent(inout) :: this
        character(len=*), intent(in) :: value

        call this%append(value)
    end subroutine push

    function pop(this) result(return_value)
        class(node), intent(inout) :: this
        character(:), allocatable :: return_value

        type(sub_node), pointer :: temp

        if (.not. associated(this%stack)) then
            ! print *, 'La pila está vacía.'
            return
        end if

        temp => this%stack
        this%stack => this%stack%next
        return_value = temp%value
        ! print *, '******** Pop:', temp%value, " ********"

        deallocate(temp)
    end function pop

    function popAtIndex(self, index) result(value_delete)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: index
        character(:), allocatable :: value_delete

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                value_delete= current%pop()
                return
            end if
            current => current%next
        end do
    end function popAtIndex

    subroutine pop_normal(this)
        class(node), intent(inout) :: this

        type(sub_node), pointer :: temp

        if (.not. associated(this%stack)) then
            ! print *, 'La pila está vacía.'
            return
        end if

        temp => this%stack
        this%stack => this%stack%next
        ! print *, '******** Pop:', temp%value, " ********"

        deallocate(temp)
    end subroutine pop_normal

    subroutine pop_inicial(self, index)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: index

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                call current%pop_normal()
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end subroutine pop_inicial

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

    subroutine asignar_datos_cliente(self, index, name_client,num_imgG, num_imgP)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: index
        integer, intent(in) :: num_imgG 
        integer, intent(in) :: num_imgP
        character(len=*), intent(in) :: name_client
        

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                current%num_imgG = num_imgG
                current%num_imgP = num_imgP
                current%name_client = name_client

                ! asignar el valor original
                current%num_imgG_original = num_imgG
                current%num_imgP_original = num_imgP

                if (len_trim(name_client) > 0) then
                    current%ocupado = .true.
                    print *, 'El cliente ', current%name_client, ' ingresa a la ventanilla ', current%index
                else
                    current%ocupado = .false.
                end if

                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end subroutine asignar_datos_cliente

end module List_of_list_vent

! program main
!     use List_of_list_vent
!     implicit none

!     type(List_of_list) :: list
!     integer:: i,num_ventanillas
!     character(len=:), allocatable :: valor_eliminado
!     logical :: pila_llena_ventanilla

!     num_ventanillas = 2
!     pila_llena_ventanilla= .false.

!     call list%insert(1, 'primero')
!     call list%insert(2, 'hola2')
!     ! call list%insert(2, 'adios2')
!     ! call list%insert(3, 'hola4')
!     ! call list%insert(3, 'adios4')
!     ! call list%insert(2, 'hola3')

!     ! call list%insert(1,'bye')

!     ! call list%printList()

!     ! call list%printPunteros()
!     ! print *, ""

!     call list%printList()

!     ! call list%printPunteros()
!     ! call list%printList()

!     call list%asignar_datos_cliente(1, 'Cliente1',2, 1)
!     call list%asignar_datos_cliente(2, 'Cliente2',3, 3)

!     call list%printList()

!     ! print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
!     call list%imagenes_a_ventanilla()
!     call list%imagenes_a_ventanilla()
!     call list%imagenes_a_ventanilla()

!     call list%printList()

!     ! Ciclo para verificar si el nodo está ocupado
!     do i=1,num_ventanillas
!         pila_llena_ventanilla=list%verificar_nodo(i)

!         if (pila_llena_ventanilla) then
!             !Ciclo usando check_stack_size para eliminar todos los elementos de la pila
!             do while (list%check_stack_size(i)>0)
!                 valor_eliminado = list%popAtIndex(i)
!                 print *, "Valor eliminado: ", valor_eliminado
!             end do
!         end if

!     end do


! end program main
