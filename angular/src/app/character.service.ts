import { Injectable } from '@angular/core';
import { ApiService } from './api.service';
import { ISheetData } from './types';
import { Observable, of, BehaviorSubject, flatMap, switchMap, map, filter } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class CharacterService {
  sheet$: BehaviorSubject<ISheetData | null> =
    new BehaviorSubject(null as (ISheetData | null));
    

  preparedSpells$$: Observable<{[origin: string]:
                                {[name: string]: BehaviorSubject<boolean>}}>
    = this.sheet$.pipe(map((sheet) => this.updatePreparedSpells(sheet as ISheetData)));

  constructor(private api: ApiService) {
    this.sheet$.subscribe((sheet) => {
      console.log('char service sheet$ sub');
      console.log(sheet)
    });
  }

  listCharacters(): Observable<string[]> {
    return this.api.listCharacters();
  }

  selectCharacter(name: string): Observable<null> {
    const select$ = this.api.loadCharacter(name).pipe(switchMap((_) => this.api.sheet()));
    select$.subscribe((sheet) => {
      this.sheet$.next(sheet);
    });
    return select$.pipe(map((_) => null));
  }

  private updatePreparedSpells(sheet: ISheetData) : {
    [origin: string]: {[name: string]: BehaviorSubject<boolean>}
  }
  {
    return Object.fromEntries(
      sheet.spellcasting_sections.map(({origin, spells, ...rest}) =>
        [origin, Object.fromEntries(spells
          .filter((spell) => spell.prepared === false)
          .map((spell) => [spell, new BehaviorSubject(false)]))])
    );
  }
}
