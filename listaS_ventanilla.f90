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
        character(:), allocatable :: name_client
        logical :: ocupado = .false.
        type(node), pointer :: next => null()
        type(sub_node), pointer :: stack => null() ! pila
        
    contains
        procedure :: append
        procedure :: push
        procedure :: pop
        procedure :: print
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
    end type List_of_list

contains

    subroutine imagenes_a_ventanilla(self)
        class(List_of_list), intent(inout) :: self

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%ocupado .and. current%num_imgG > 0) then
                ! Agregar una imagen grande
                call self%insert(current%index, 'imagen Grande')
                ! Decrementar el contador de imágenes grandes
                current%num_imgG = current%num_imgG - 1

            else if (current%ocupado .and. current%num_imgP > 0) then
                ! Agregar una imagen grande
                call self%insert(current%index, 'imagen Pequena')
                ! Decrementar el contador de imágenes grandes
                current%num_imgP = current%num_imgP - 1
            end if
            current => current%next
        end do

        ! Si no se encontró ningún nodo ocupado con imágenes grandes, se imprime un mensaje.
        print *, 'Se han agregado imágenes grandes a todos los nodos ocupados en la lista.'
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

    subroutine pop(this)
        class(node), intent(inout) :: this

        type(sub_node), pointer :: temp

        if (.not. associated(this%stack)) then
            print *, 'La pila está vacía.'
            return
        end if

        temp => this%stack
        this%stack => this%stack%next

        print *, '******** Pop:', temp%value, " ********"

        deallocate(temp)
    end subroutine pop

    subroutine popAtIndex(self, index)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: index

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                call current%pop()
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end subroutine popAtIndex

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

                if (len_trim(name_client) > 0) then
                    current%ocupado = .true.
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

!     ! ! pop 
!     ! call list%popAtIndex(1)

!     ! call list%printPunteros()
!     ! call list%printList()

!     call list%asignar_datos_cliente(1, 'Cliente1',2, 1)
!     call list%asignar_datos_cliente(2, 'Cliente2',3, 3)

!     call list%printList()

!     print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
!     call list%imagenes_a_ventanilla()
!     call list%imagenes_a_ventanilla()
!     call list%imagenes_a_ventanilla()
!     call list%imagenes_a_ventanilla()

!     call list%printList()


! end program main
