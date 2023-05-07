import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, of, pipe, map } from 'rxjs';
import { ISheetData } from './types';

@Injectable({
  providedIn: 'root'
})
export class ApiService {

  constructor(private http: HttpClient) { }

  listCharacters(): Observable<string[]> {
    return this.http.get<{list: string[]}>('http://localhost:8000/request/list_characters', {
      headers: new HttpHeaders({
        'Accept': 'application/json'
      })
    }).pipe(map(obj => obj.list));
  }

  loadCharacter(name: string): Observable<string> {
    console.log('ApiService.loadCharacter');
    return this.http.get<string>(
      'http://localhost:8000/request/load_character?name=' + name, {
      headers: new HttpHeaders({
        'Accept': 'application/json'
      })
    });
  }

  sheet(): Observable<ISheetData> {
    console.log('ApiService.sheet');
    return this.http.get<ISheetData>(
      'http://localhost:8000/request/sheet', {
      headers: new HttpHeaders({
        'Accept': 'application/json'
      })
    });
  }
}
