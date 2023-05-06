import { Component, OnInit } from '@angular/core';
import { CharacterService } from '../character.service'
import { Observable, of } from 'rxjs';

@Component({
  selector: 'app-character-selection',
  templateUrl: './character-selection.component.html',
  styleUrls: ['./character-selection.component.css']
})
export class CharacterSelectionComponent implements OnInit {
  newCharName: string = '';
  characterList$: Observable<string[]> = of([]);

  constructor(private characterService: CharacterService) {}

  ngOnInit(): void {
    this.characterList$ = this.characterService.listCharacters();
    this.characterList$.subscribe(list => console.log(list));
  }
  
  selectChar(name: string) {
    this.characterService.selectCharacter(name).subscribe((_) =>
      window.location.href = 'sheet'
    );
  }
}
