<?php

use App\Http\Controllers\LivrosController;
use Illuminate\Support\Facades\Route;

Route::get('/', function () {
    return view('welcome');
});
Route::post('/livros/criar', [LivrosController::class, 'create']);
Route::get('/criar-livro', function () {
    return view('criar-livro');
});