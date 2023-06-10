import { Injectable } from '@angular/core';
import { ApiService } from './api.service';
import { ISheetData } from './types';
import { Observable, of, BehaviorSubject, flatMap, switchMap, map, tap, filter, merge, scan } from 'rxjs';

type SpellPreparednessUpdate = {
  origin: string,
  name: string,
  preparedness: boolean
};
type SpellPreparednessTable = {[origin: string]: {[name: string]: boolean}};
type SpellPreparednessInput
  = {tag: 'sheet', sheet: ISheetData}
  | {tag: 'update', update: SpellPreparednessUpdate};

@Injectable({
  providedIn: 'root'
})
export class CharacterService {
  sheet$: BehaviorSubject<ISheetData | null> =
    new BehaviorSubject(null as (ISheetData | null));

  private setSpellPreparedness$: BehaviorSubject<SpellPreparednessUpdate>
    = new BehaviorSubject(({origin: '', name: '', preparedness: false} as SpellPreparednessUpdate));

  private spellPreparednessInputs$: Observable<SpellPreparednessInput> =
    merge(
      this.sheet$.pipe(
        filter((sheet) => typeof sheet !== null),
        map((sheet) => ({tag: 'sheet', sheet: sheet}))
      ) as Observable<SpellPreparednessInput>,
      this.setSpellPreparedness$.pipe(
        filter((update) => update.origin !== ''),
        map((update) => ({tag: 'update', update: update}))
      ) as Observable<SpellPreparednessInput>);

  preparedSpells$: Observable<SpellPreparednessTable> =
    (this.spellPreparednessInputs$
      .pipe(scan((acc, value, index) => this.updateSpellPreparednessTable(acc as SpellPreparednessTable, value), {}))) as (Observable<SpellPreparednessTable>);

  private updateSpellPreparednessTable(
    table: SpellPreparednessTable,
    input: SpellPreparednessInput
  ) : SpellPreparednessTable
  {

    const result = input.tag === 'sheet'
      ?  this.initializeSpellPreparednessTable(input.sheet)
      :  Object.assign({}, table,
                       {[input.update.origin]:
                        {[input.update.name]:
                         input.update.preparedness}});
    console.log('in char service update fn');
    console.log(input);
    console.log(result);
    return result;
  }

  constructor(private api: ApiService) {
    this.sheet$.subscribe((sheet) => {
      console.log('char service sheet$ sub');
      console.log(sheet)
    });
  }

  listCharacters(): Observable<string[]> {
    return this.api.listCharacters();
  }

  loadCharacter(name: string): Observable<string> {
    return this.api.loadCharacter(name);
  }

  refresh(): Observable<ISheetData> {
    const req = this.api.sheet();
    req.subscribe((sheet) => this.sheet$.next(sheet));
    return req;
  }

  setSpellPreparedness(origin: string, name: string, preparedness: boolean): void {
    this.setSpellPreparedness$.next({origin, name, preparedness});
  }

  private initializeSpellPreparednessTable(sheet: ISheetData): SpellPreparednessTable {
    return Object.fromEntries(
      sheet.spellcasting_sections.map(({origin, spells, ...rest}) =>
        [origin, Object.fromEntries(spells
          .filter((spell) => spell.prepared === false)
          .map((spell) => [spell, false]))]));
  }
}
