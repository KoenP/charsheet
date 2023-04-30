import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service'
import { Observable, of } from 'rxjs';

@Component({
  selector: 'app-character-selection',
  templateUrl: './character-selection.component.html',
  styleUrls: ['./character-selection.component.css']
})
export class CharacterSelectionComponent implements OnInit {
  newCharName: string = "";
  characterList$: Observable<string[]> = of([]);

  constructor(private api: ApiService) {}

  ngOnInit(): void {
    this.characterList$ = this.api.listCharacters();
    this.characterList$.subscribe(list => console.log(list));
  }
  
  selectChar(char: string) {
    this.api.loadCharacter(char).subscribe(_ => 
      window.location.href = 'sheet'
    );
  }
}
