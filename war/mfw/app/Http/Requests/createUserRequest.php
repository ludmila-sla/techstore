<?php

namespace App\Http\Requests;

use Illuminate\Foundation\Http\FormRequest;

class createUserRequest extends FormRequest
{
    /**
     * Determine if the user is authorized to make this request.
     */
    public function authorize(): bool
    {
        return true;
    }

    /**
     * Get the validation rules that apply to the request.
     *
     * @return array<string, \Illuminate\Contracts\Validation\ValidationRule|array<mixed>|string>
     */
    public function rules(): array
    {
        return [
            "nome" => "required|string",
            "celular" => "required|string",
            "email" => "required|string",
            "user" => "required|string",
            "foto" => "required|string",
            "idade" => "required|int",
            "pix_tipo" => "sometimes|string",
            "pix_num" => "sometimes|string"

        ];
    }
}
