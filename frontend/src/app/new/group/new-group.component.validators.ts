import { AbstractControl, ValidatorFn } from '@angular/forms';

const canonicalizable = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ";

export function canonicalizableValidator(): ValidatorFn {
    return (control: AbstractControl): {[key: string]: any} | null => {
        const error = (Array.from(control.value) as string[]).some(c => canonicalizable.indexOf(c) < 0);
        return error ? {canonicalization: {value: control.value}} : null;
    };
}
