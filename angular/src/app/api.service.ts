import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ApiService {

  constructor(private http: HttpClient) { }

  listCharacters(): Observable<string[]> {
    return this.getRequest('request/list_characters');
  }

  getRequest(path: string): Observable<any> {
    return this.http.get('localhost:8000/' + path, {
      headers: new HttpHeaders({
        'Content-Type': 'json',
        'Access-Control-Allow-Origin': '*'
      })
    });
    // TODO hier zat ik: CORS bullshit
  }
}
