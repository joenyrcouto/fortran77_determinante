      program tarefa5 !autor Joenyr :0

      real v(362880, 10), v2(362880, 10), A(10,9,9), det(10),
     & produto, y(9), x(9)
      integer i, j, s, s2, k/200/, nlinha/0/, ncoluna/0/, onoff/0/,
     & fatorial/1/, col_paridade, N
      character*200 linha

c     Ler permutação base
      open(UNIT=10, FILE='tarefa-5-entrada-1-15464382.txt')

      !Contar linha
      do
        read(10, *, end=20)
        nlinha = nlinha+1
      end do

      !Salvar string
20    rewind(10)
      read(10, '(A)') linha

      !Contar colunas sem espaço
21    if (linha(k:k) .ne. ' ') then
        k = k-1
        if (k .gt. 0) goto 21
      end if

      !Contar qnt de numeros
      do j=1,k
        if (linha(j:j) .ne. ' ') then
          if (onoff .eq. 0) then
            ncoluna = ncoluna+1
            onoff = 1
          end if
        else
          onoff = 0
        end if
      end do

      !Ler numeros
      rewind(10)
      do i=1,nlinha
        read(10,*) (v(i,j), j=1,ncoluna)
      end do
      close(10)

c     Ler matriz e vetor alvo

      !Salvar string
      open(UNIT=11, FILE='matriz.txt')
      read(11, '(A)') linha

      k = 200
      !Contar colunas sem espaço
22    if (linha(k:k) .ne. ' ') then
        k = k-1
        if (k .gt. 0) goto 22
      end if

      onoff = 0
      !Contar qnt de numeros
      do j=1,k
        if (linha(j:j) .ne. ' ') then
          if (onoff .eq. 0) then
            N = N+1
            onoff = 1
          end if
        else
          onoff = 0
        end if
      end do

      !Ler numeros da matriz
      rewind(11)
      do i=1,N
        read(11,*) (A(1,i,j), j=1,N)
      end do

      !Ler numeros do vetor
      do i=1,N
        read(11,*) y(i)
      end do

c     Gerar outras matrizes

      do s=2,N+1
        do i=1,N
          do j=1,N
            if (j .ne. s-1) then
              A(s,i,j) = A(1, i,j)
            else
              A(s,i,j) = y(i)
            end if
          end do
        end do
      end do

c     Gerar permutações

23    if ((ncoluna-1) .lt. N) then

      !Copiar vetores
      do i=1,nlinha
        do s=(i-1)*ncoluna+1,i*ncoluna
          do j=1,ncoluna
            v2(s,j) = v(i,j)
          end do
        end do
      end do

      !Organizar novo vetor
      do i=1,nlinha*ncoluna
        do j=0,mod(i-1,ncoluna)
          v2(i,ncoluna-j+1) = v2(i,ncoluna-j)
        end do
      end do

      !Adicionar número N+1 a matriz e arrumar paridade
      do i=1,nlinha*ncoluna
        v2(i,ncoluna-mod(i-1,ncoluna)) = ncoluna
        v2(i,ncoluna+1) = (-1)**(mod(i+1,2))*v2(i,ncoluna+1)
      end do

      nlinha = nlinha*ncoluna
      ncoluna = ncoluna+1

      end if

c     Gerar permutações de ordens maiores

      if ((ncoluna-1) .lt. N) then

      !Copiar vetores
      do i=1,nlinha
        do s=(i-1)*ncoluna+1,i*ncoluna
          do j=1,ncoluna
            v(s,j) = v2(i,j)
          end do
        end do
      end do

      !Organizar novo vetor
      do i=1,nlinha*ncoluna
        do j=0,mod(i-1,ncoluna)
          v(i,ncoluna-j+1) = v(i,ncoluna-j)
        end do
      end do

      !Adicionar número N+1 a matriz e arrumar paridade
      do i=1,nlinha*ncoluna
        v(i,ncoluna-mod(i-1,ncoluna)) = ncoluna
        v(i,ncoluna+1) = (-1)**(mod(ncoluna-mod(i-1,ncoluna)+1,2))*
     & v(i,ncoluna+1)
      end do

      nlinha = nlinha*ncoluna
      ncoluna = ncoluna+1

      goto 23
      end if

c     Calcular determinantes

      do s2=1,N+1

      col_paridade = ncoluna
      do i = 1, nlinha
          !Calcula o produto
          produto = 1.0
          do s = 1, N
            if (mod(N,2) .eq. 0) then
                produto = produto * A(s2, s, int(v2(i,s)))
              else
                produto = produto * A(s2, s, int(v(i,s)))
             end if
          end do

          !Adiciona ao determinante com a paridade
          if (mod(N,2) .eq. 0) then
             det(s2) = det(s2) + v2(i, col_paridade) * produto
          else
             det(s2) = det(s2) + v(i, col_paridade) * produto
          end if
      end do

      end do

c     Calcular soluções

      do i = 1,N
        x(i) = det(1+i)/det(1)
      end do

c     Imprimir soluções

      write(*,31) (x(j), j=1,N)
31    format("Soluções: ", 9F10.2)

      stop
      end
