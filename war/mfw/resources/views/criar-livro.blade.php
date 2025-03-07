<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Criar Livro</title>
    <link rel="stylesheet" href="{{ asset('css/styles.css') }}">
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
                <a href="#">Opção 1</a>
                <a href="#">Opção 2</a>
                <a href="#">Opção 3</a>
            </div>
        </div>
    </div>

    <!-- Conteúdo principal -->
    <div class="container">
        <h1>Criar Novo Livro</h1>
        <form id="livroForm" enctype="multipart/form-data">
            <div class="form-group">
                <label for="nome">Nome:</label>
                <input type="text" id="nome" name="nome" required>
            </div>
            <div class="form-group">
                <label for="celular">Celular:</label>
                <input type="text" id="celular" name="celular" required>
            </div>
            <div class="form-group">
                <label for="email">Email:</label>
                <input type="email" id="email" name="email" required>
            </div>
            <div class="form-group">
                <label for="user">User:</label>
                <input type="text" id="user" name="user" required>
            </div>
            <div class="form-group">
                <label for="foto">Foto:</label>
                <input type="file" id="foto" name="foto" accept="image/*" required>
            </div>
            <div class="form-group">
                <label for="idade">Idade:</label>
                <input type="number" id="idade" name="idade" required>
            </div>
            <div class="form-group">
                <label for="pix_tipo">Tipo de PIX (opcional):</label>
                <input type="text" id="pix_tipo" name="pix_tipo">
            </div>
            <div class="form-group">
                <label for="pix_num">Número do PIX (opcional):</label>
                <input type="text" id="pix_num" name="pix_num">
            </div>
            <button type="submit" class="btn">Criar Livro</button>
        </form>
        <div id="message" class="message"></div>
    </div>

    <!-- Barra inferior -->
    <div class="bottom-bar"></div>

    <script>
        document.getElementById('livroForm').addEventListener('submit', function (e) {
            e.preventDefault();

            const formData = new FormData(this);

            fetch('/api/livros/criar', {
                method: 'POST',
                body: formData
            })
            .then(response => response.json())
            .then(result => {
                const messageDiv = document.getElementById('message');
                if (result.success) {
                    messageDiv.textContent = result.message;
                    messageDiv.className = 'message success';
                } else {
                    messageDiv.textContent = result.message;
                    messageDiv.className = 'message error';
                }
            })
            .catch(error => {
                console.error('Erro:', error);
                const messageDiv = document.getElementById('message');
                messageDiv.textContent = 'Erro ao enviar os dados.';
                messageDiv.className = 'message error';
            });
        });
    </script>
</body>
</html>