import { Injectable } from '@angular/core';
import { FormControl, FormGroup } from '@angular/forms';


@Injectable({
  providedIn: 'root'
})
export class CustomValidators {
  SpecialCharacters(controlName: string) {
    const nameRegexp: RegExp = /[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/;

    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];
      if (control.errors && !control.errors?.['invalidName']) {
        // return if another validator has already found an error on the control
        return;
      }
      // set error on control if validation fails
      try {
        if (control.value && nameRegexp.test(control.value)) {
          control.setErrors({ invalidName: true });
        }else{
          control.setErrors(null);
        }
      } catch (_) {
        control.setErrors(null);
      }
    };
  }
  MustMatch(controlName: string, matchingControlName: string) {
    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];
      const matchingControl = formGroup.controls[matchingControlName];

      if (matchingControl.errors && !matchingControl.errors?.['mustMatch']) {
        // return if another validator has already found an error on the matchingControl
        return;
      }

      // set error on matchingControl if validation fails
      if (control.value !== matchingControl.value) {
        matchingControl.setErrors({ mustMatch: true });
      } else {
        matchingControl.setErrors(null);
      }
    }
  }
  PasswordMust(controlName: string, ) {
    const regexp: RegExp = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$/;

    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];
      if (control.errors && !control.errors?.['invalidPassMust']) {
        // return if another validator has already found an error on the control
        return;
      }
      // set error on control if validation fails
      try {
        if (control.value && regexp.test(control.value)) {
          control.setErrors(null);
        } else {
          control.setErrors({ invalidPassMust: true });
        }
      } catch (_) {
        control.setErrors(null);
      }
    };
  }
  URLValidator(controlName: string) {
    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];

      if (control.errors && !control.errors?.['invalidURL']) {
        // return if another validator has already found an error on the control
        return;
      }

      // set error on control if validation fails
      try {
        const url = new URL(control.value);
        control.setErrors(null);
      } catch (_) {
        control.setErrors({ invalidURL: true });
      }
    };
  }
  dateRegexValidator(controlName: string){
  const regexPattern = /^(([1-9])|([0][1-9])|([1-2][0-9])|([3][0-1]))(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\d{4}$/gi;

    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];
      if (control.errors && !control.errors?.['invalidDate']) {
        // return if another validator has already found an error on the control
        return;
      }
      // set error on control if validation fails
      try {
        if (control.value && regexPattern.test(control.value)) {
          control.setErrors({ invalidDate: true });
        } else {
          control.setErrors(null);
        }
      } catch (_) {
        control.setErrors(null);
      }
    };
  }
  NumbericRegex(controlName: string) {
    const numberRegEx = /\(?([0-9]{3})\)?([ .-]?)([0-9]{3})\2([0-9]{4})/;

    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];
      if (control.errors && !control.errors?.['invalidNumberic']) {
        // return if another validator has already found an error on the control
        return;
      }
      // set error on control if validation fails
      try {
        if (control.value && numberRegEx.test(control.value)) {
          control.setErrors(null);
        } else {
          control.setErrors({ invalidNumberic: true });

        }
      } catch (_) {
        control.setErrors(null);
      }
    };
  }
  codeValidator(controlName: string) {
    const regexp = /[a-zA-Z0-9 .-_]{3,255}/;
    return (formGroup: FormGroup) => {
      const control = formGroup.controls[controlName];
      if(control.errors && !control.errors?.['invalidCode']) {
        return;
      }

      try {
        if(control.value && regexp .test(control.value)) {
          control.setErrors(null);
        }else {
          control.setErrors({ invalidCode:true });
        }
      }catch(_) {
        control.setErrors(null);
      }
    }
  }
}
