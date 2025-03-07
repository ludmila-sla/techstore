<?php

namespace App\Http\Controllers;

use App\Http\Requests\LivroRequest;
use Illuminate\Support\Facades\DB;
use Illuminate\Http\Request;
use Ramsey\Uuid\Uuid;

class LivrosController extends Controller
{
    public function create(LivroRequest $request, $user)
    {
        $data = $request->all();
        $data['_id'] = Uuid::uuid4()->toString();
        $data["user"] = $user;
        $created = DB::table('livros')->insertOrUpdate($data);
        return response($created, $created ? 202 : 400);
    }
    public function list(Request $request)
    {
        $query = DB::table('livros');
        $genero = $request->genero;
        $buscar = $request->buscar;
        $maisLidos = $request->maisLidos;
        $recentes = $request->recentes;
        $melhoresAva = $request->melhoresAva;
        $paginate = $request->paginate ?? 10;
        if ($genero || $buscar) {
            $query->where(function ($query) use ($genero, $buscar) {
                $query->where('generos', 'LIKE',  '%' . $genero .  '%')
                    ->orWhere('name', 'LIKE',  '%' . $buscar .  '%')
                    ->orWhere('generos', 'LIKE',  '%' . $buscar .  '%')
                    ->orWhere('tags', 'LIKE',  '%' . $buscar .  '%');
            });
        }
        if ($maisLidos) {
            $query->orderBy('visualizacoes', 'desc');
        }
    
        if ($recentes) {
            $query->orderBy('created_at', 'desc');
        }
    
        if ($melhoresAva) {
            $query->orderBy('avaliacao', 'desc');
        }
        $livro = $query->paginate($paginate);
        return response($livro, $livro ? 200 : 404);
    }
    public function show($id)
    {
        $livro = DB::table('livros')->where('_id', $id)->first();
        return response($livro, $livro ? 200 : 404);
    }
    public function delete($id)
    {
        $deleted = DB::table('livros')->where('_id', $id)->delete();
        return response($deleted, $deleted ? 200 : 400);
    }
}
