<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Home</title>
    <link rel="stylesheet" href="{{ asset('css/styles.css') }}">
    <!-- Biblioteca para o carrossel (Slick Carousel) -->
    <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick.css"/>
    <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick-theme.css"/>
</head>
<body>
    <!-- Barra superior -->
    <div class="top-bar">
        <div class="logo">
            <img src="{{ asset('storage/logo.png') }}" alt="Logo">
        </div>
        <div class="menu">
            <button class="menu-button">Menu</button>
            <div class="menu-content">
                <a href="/">Home</a>
                <a href="/criar-livro">Criar Livro</a>
                <a href="#">Opção 3</a>
            </div>
        </div>
    </div>

    <!-- Conteúdo principal -->
    <div class="container">
        <h1>Livros Mais Lidos</h1>
        <div class="carrossel" id="carrossel-mais-lidos">
            <!-- Itens do carrossel serão preenchidos via JavaScript -->
        </div>

        <h1>Livros Mais Recentes</h1>
        <div class="carrossel" id="carrossel-mais-recentes">
            <!-- Itens do carrossel serão preenchidos via JavaScript -->
        </div>

        <h1>Melhores Avaliados</h1>
        <div class="carrossel" id="carrossel-melhores-avaliados">
            <!-- Itens do carrossel serão preenchidos via JavaScript -->
        </div>
    </div>

    <!-- Barra inferior -->
    <div class="bottom-bar"></div>

    <!-- Scripts -->
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/slick-carousel@1.8.1/slick/slick.min.js"></script>
    <script>
        // Função para carregar os livros
        async function carregarLivros(categoria, elementoId) {
            try {
                const response = await fetch(`/api/livros/list?${categoria}=true`);
                const livros = await response.json();

                const carrossel = document.getElementById(elementoId);
                carrossel.innerHTML = livros.data.map(livro => `
                    <div class="livro-item">
                        <img src="${livro.foto}" alt="${livro.nome}">
                        <h3>${livro.nome}</h3>
                        <p>${livro.generos}</p>
                    </div>
                `).join('');

                // Inicializa o carrossel
                $(`#${elementoId}`).slick({
                    dots: true,
                    infinite: true,
                    speed: 300,
                    slidesToShow: 4,
                    slidesToScroll: 1,
                    responsive: [
                        {
                            breakpoint: 1024,
                            settings: {
                                slidesToShow: 3,
                                slidesToScroll: 1,
                            }
                        },
                        {
                            breakpoint: 600,
                            settings: {
                                slidesToShow: 2,
                                slidesToScroll: 1,
                            }
                        }
                    ]
                });
            } catch (error) {
                console.error(`Erro ao carregar ${categoria}:`, error);
            }
        }

        // Carrega os carrosséis ao abrir a página
        document.addEventListener('DOMContentLoaded', function () {
            carregarLivros('maisLidos', 'carrossel-mais-lidos');
            carregarLivros('recentes', 'carrossel-mais-recentes');
            carregarLivros('melhoresAva', 'carrossel-melhores-avaliados');
        });
    </script>
</body>
</html>