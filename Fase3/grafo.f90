module List_of_list_m
    implicit none
    private
    public:: iniciar_tabla,prueba_tabla

    ! Variables del grafo
    type :: sub_node
        integer :: value
        integer :: distance
        integer :: impresoras
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node
        integer :: index
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(sub_node), pointer :: list => null()
    contains
        procedure :: append
        procedure :: print
    end type node

    type, public :: List_of_list
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        ! procedure :: append
        procedure :: insert
        procedure :: printList
        procedure :: graph
    end type List_of_list

    type :: Table_Entry
        integer :: index
        integer :: valor_temporal
        integer :: valor_final
    end type Table_Entry

    type(table_entry), dimension(:), allocatable :: table

    contains


   !Subrutinas para Dijkstra
    subroutine prueba_tabla(self)
        class(List_of_list), intent(in) :: self
        type(node), pointer :: current_node
        type(sub_node), pointer :: current_subnode
        integer :: SIZE, i, j

        ! Obtener el tamaño de la tabla
        SIZE = 0
        ! Recorrer la lista principal
        current_node => self%head
        do while(associated(current_node))
            SIZE = SIZE + 1

            ! Recorrer la sublista para el ultimo valor que esta en los subnodos
            current_subnode => current_node%list
            do while(associated(current_subnode))

                ! current_node%index
                ! current_subnode%value 
                
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do

        !Un espacio extra para el ultimo nodo sin conexiones
        SIZE=SIZE+1

        print *, "El tamaño de la tabla es de: ", SIZE

        ! Asignar el tamaño del array
        allocate(table(SIZE))

        !Se inicializa el espacio extra en -1 para indicar el infinito
        table(SIZE)%index=-1

        ! Inicializar los campos "valor_temporal" y "valor_final"
        i = 0
        current_node => self%head
        do while(associated(current_node))
            i = i + 1
            table(i)%index = current_node%index

            current_node => current_node%next
        end do

        !se recorren nuevamente todos los nodos, y los subnodos y se compara con la tabla para ver si falta alguno
        current_node => self%head
        do while(associated(current_node))
            current_subnode => current_node%list
            do while(associated(current_subnode))
                do j=1,SIZE
                    if(current_subnode%value==table(j)%index) then
                        exit
                    else if(j==SIZE) then
                        i=i+1
                        table(i)%index=current_subnode%value
                    end if
                end do
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do

        ! Imprimir la tabla
        print *, "Tabla de dijkstra:"
        do j = 1, SIZE
            print *, "Indice: ", j, ", Valor: ", table(j)%index
        end do
    end subroutine prueba_tabla
    

    




   !Subrutinas para Dijkstra
    subroutine iniciar_tabla(self)
        class(List_of_list), intent(in) :: self
        type(node), pointer :: current_node
        type(sub_node), pointer :: current_subnode
        integer :: SIZE, i, j

        ! SIZE = 0
        ! current_node => self%head
        ! do while(associated(current_node))
        !     SIZE = SIZE + 1
        !     current_node => current_node%next
        ! end do

        ! print *, "El tamaño de la tabla es de: ", SIZE

        ! Obtener el tamaño de la tabla
        SIZE = 0
        ! Recorrer la lista principal
        current_node => self%head
        do while(associated(current_node))
            SIZE = SIZE + 1

            ! Recorrer la sublista para el ultimo valor que esta en los subnodos
            current_subnode => current_node%list
            do while(associated(current_subnode))

                ! current_node%index
                ! current_subnode%value 
                
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do

        print *, "El tamaño de la tabla es de: ", SIZE

        ! Asignar el tamaño del array
        allocate(table(SIZE))

        ! Inicializar los campos "valor_temporal" y "valor_final"
        i = 0
        current_node => self%head
        do while(associated(current_node))
            i = i + 1
            table(i)%index = current_node%index
            table(i)%valor_temporal = 0 ! Se inicializa en 0 para evitar errores
            table(i)%valor_final = 0 ! Se inicializa en 0 para evitar errores

            current_node => current_node%next
        end do

        ! Imprimir la tabla
        print *, "Tabla de dijkstra:"
        do j = 1, SIZE
            print *, "Indice: ", j, ", Valor: ", table(j)%index, ", Valor Temporal: ",&
             table(j)%valor_temporal, ", Valor Final: ", table(j)%valor_final
        end do
    end subroutine iniciar_tabla


    ! Subrutinas para la lista de listas
    subroutine insert(self, index, value, distance, impresoras)
        class(List_of_list), intent(inout) :: self
        integer, intent(in) :: value
        integer, intent(in) :: index
        integer, intent(in) :: distance
        integer, intent(in) :: impresoras

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            allocate(aux)
            aux%index = index
            self%head => aux
            self%tail => aux
            call aux%append(value, distance, impresoras)
        else
            if(index < self%head%index) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%index = index
                call new%append(value, distance, impresoras)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                            call aux%append(value, distance, impresoras)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%index = index
                            call new%append(value, distance, impresoras)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then
                    call aux%append(value, distance, impresoras)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%index = index
                    call new%append(value, distance, impresoras)
                end if
            end if
        end if
    end subroutine insert

    subroutine printList(self)
        class(List_of_list) :: self
        type(Node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'Indice: ', aux%index
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    !Subrutina y funciones de sub nodo
    subroutine append(self, string, distance, impresoras)
        class(node), intent(inout) :: self
        integer, intent(in) :: string
        integer, intent(in) :: distance
        integer, intent(in) :: impresoras

        type(sub_node), pointer :: aux
        type(sub_node), pointer :: new

        allocate(new)
        new%value = string
        new%distance = distance
        new%impresoras = impresoras

        if(.not. associated(self%list)) then
            self%list => new
        else
            aux => self%list
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => new
        end if
    end subroutine append


    ! subroutine print(self)
    !     class(node), intent(inout) :: self

    !     type(sub_node), pointer :: aux
    !     aux => self%list

    !     do while(associated(aux))
    !         print *, aux%value
    !         aux => aux%next
    !     end do
    ! end subroutine 

    subroutine print(self)
        class(node), intent(inout) :: self

        type(sub_node), pointer :: aux
        aux => self%list

        do while(associated(aux))
            print *, "Valor: ", aux%value, ", Distancia: ", aux%distance, ", Impresoras: ", aux%impresoras
            aux => aux%next
        end do
    end subroutine print

    
    subroutine graph(self)
        class(List_of_list), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: command
        type(node), pointer :: current_node
        type(sub_node), pointer :: current_subnode
    
        command = "dot -Tpng ./graph.dot -o ./graph.png"
        io = 1
    
        ! Abrir archivo DOT
        open(newunit=io, file="./graph.dot")
        write(io, *) "digraph G {"
    
        ! Recorrer la lista principal
        current_node => self%head
        do while(associated(current_node))
            ! Escribir el nodo principal
            write(io, *) current_node%index, "[label = """, current_node%index, """]"
            
            ! Recorrer la sublista
            current_subnode => current_node%list
            do while(associated(current_subnode))
                ! Escribir el subnodo y la conexión bidireccional con el nodo principal
                write(io, *) current_node%index, " -> ", current_subnode%value, " [dir = normal];"
                current_subnode => current_subnode%next
            end do
            current_node => current_node%next
        end do
    
        write(io, *) "}"
        close(io)
    
        ! Ejecutar Graphviz para generar la imagen
        call execute_command_line(command, exitstat=i)
    
        if (i /= 0) then
            print *, "Error al crear la imagen."
        else
            print *, "Imagen creada con éxito."
        end if
    end subroutine graph


end module List_of_list_m

program main
    use List_of_list_m

    type(List_of_list) :: adjacency_list
    integer :: distance,impresoras

    integer:: A, B, C, D, E, F, G, H, I, J, K, L, M, N, P

    ! Insertar algunos datos de ejemplo

    A= 1
    B= 2
    C= 3
    D= 4
    E= 5
    F= 6
    G= 7
    H= 8
    I= 9
    J= 10
    K= 11
    L= 12
    M= 18
    N= 15
    P= 20

    distance=8
    impresoras=2
    call adjacency_list%insert(A, B,distance,impresoras)

    call adjacency_list%insert(A, E,4,6)

    call adjacency_list%insert(A, D,7,impresoras)

    call adjacency_list%insert(B, C,3,6)

    call adjacency_list%insert(B,F,4,7)

    call adjacency_list%insert(B,E,12,1)

    call adjacency_list%insert(C,F,9,2)

    call adjacency_list%insert(C,G,11,8)

    call adjacency_list%insert(D,E, 9,7)

    call adjacency_list%insert(D,H,6,4)

    call adjacency_list%insert(E,F,3,3)

    call adjacency_list%insert(E,J,5,7)

    call adjacency_list%insert(E,I,8,6)

    call adjacency_list%insert(F,G,1,8)

    call adjacency_list%insert(F,K,8,5)

    call adjacency_list%insert(G,K,8,3)

    call adjacency_list%insert(G,L,7,1)

    call adjacency_list%insert(H,I,2,8)

    call adjacency_list%insert(H,M,7,2)

    call adjacency_list%insert(I,J,10,3)

    call adjacency_list%insert(I,M,6,10)

    call adjacency_list%insert(J,K,6,4)

    call adjacency_list%insert(J,N,9,3)

    call adjacency_list%insert(K,L,5,3)

    call adjacency_list%insert(K,P,7,2)

    call adjacency_list%insert(L,P,6,11)

    call adjacency_list%insert(M,N,2,5)

    call adjacency_list%insert(N,P,12,7)


    ! ! Imprimir la lista de adyacencia
    ! print *, "Lista de adyacencia:"
    ! call adjacency_list%printList()

    ! ! Generar el archivo DOT y la imagen del grafo
    ! call adjacency_list%graph()

    ! print *, "Se ha generado la imagen del grafo."

    ! call iniciar_tabla(adjacency_list)

    ! Llamada al método para establecer el tamaño

    call prueba_tabla(adjacency_list)

end program main
