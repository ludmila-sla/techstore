<?php

namespace App\Http\Controllers;

use App\Http\Requests\createUserRequest;
use Illuminate\Support\Facades\DB;
use Illuminate\Http\Request;
use Ramsey\Uuid\Uuid;

class userController extends Controller
{
    public function create(createUserRequest $request)
    {
        $data = $request->all();
        $data['_id'] = Uuid::uuid4()->toString();
        $created = DB::table('user')->insertOrUpdate($data);
        return response($created, $created ? 202 : 400);
    }
    public function list(Request $request)
    {
        $query = DB::table('user');
        $nome = $request->nome;
        $user = $request->document;
        $paginate = $request->paginate ?? 10;
        if ($user || $nome) {
            $query->where(function ($query) use ($nome, $user) {
                $query->where('user','LIKE',  '%' . $user.  '%')
                    ->orWhere('name', 'LIKE',  '%' . $nome.  '%');
            });
        }
        $profile = $query->paginate($paginate);
        return response($profile, $profile ? 200 : 404);
    }
    public function show($id)
    {
        $profile = DB::table('user')->where('_id', $id)->first();
        $profile["livros"] = DB::table('livros')->where('user_id', $id)->get();
        return response($profile, $profile ? 200 : 404);
    }
    public function delete($id)
    {
        $deleted = DB::table('user')->where('_id', $id)->delete();
        DB::table('livros')->where('user_id', $id)->delete();
        return response($deleted, $deleted ? 200 : 400);
    }
}
