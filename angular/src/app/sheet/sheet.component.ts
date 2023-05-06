import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service'
import { ISheetData, PrologTerm, ISpell, Ability } from '../types';
import { Observable, of, BehaviorSubject, map } from 'rxjs';

@Component({
  selector: 'app-sheet',
  templateUrl: './sheet.component.html',
  styleUrls: ['./sheet.component.css']
})
export class SheetComponent {
  sheet$: Observable<ISheetData> = this.api.sheet();
  showOnlyPreparedSpells: boolean = true;
  showOnlyPreparedSpellsToggled$: BehaviorSubject<boolean> = new BehaviorSubject(true);

  constructor(private api: ApiService) {}

  formatModifier(mod: number) {
    return mod >= 0 ? '+' + mod : mod
  }

  abilities(): Ability[] {
    return ['str','dex','con','wis','int','cha'];
  }

  rincl(low: number, high: number): number[] {
    return Array.from(Array((high-low)+1)).map((_,i) => i + low);
  }

  getSpellSlotsAtSpellLevel(sheet: ISheetData, level: number) {
    const n = sheet.spell_slots[level - 1];
    return n === undefined ? 0 : n;
  }

  toggleShowOnlyPreparedSpells(val: boolean) {
    this.showOnlyPreparedSpellsToggled$.next(val);
  }

  filteredSpells$(spells: ISpell[]): Observable<ISpell[]> {
    return this.showOnlyPreparedSpellsToggled$.pipe(
      map((onlyPrepared: boolean) => onlyPrepared
        ? spells.filter((spell) => spell.prepared)
        : spells)
    );
  }

////////////////////////////////////////////////////////////////////////////////

  formatToHitOrDc(spell: ISpell): string {
    const list = this.notNullSingleton(spell.to_hit)
      .map(this.formatModifier)
      .concat(this.notNullSingleton(spell.dc)
      .map(dc => `DC ${dc} (${spell.dc_abi})`));
    return list.join(",");
  }
  
  formatComponents(cpts: PrologTerm[]): string {
    const reallyThis: SheetComponent = this;
    function formatComponent(cpt: PrologTerm): string {
      return typeof cpt === "string"
        ? cpt
        : reallyThis.htmlTooltip(cpt.functor, cpt.args[0] as string)
    }
    return cpts.map(formatComponent).join('')
  }
  
  htmlTooltip(content: string, tooltiptext: string): string {
    return `<div class="tooltip">${content}<span class="tooltiptext">${tooltiptext}</span></div>`
  }
  
  formatSpellDescription(desc: string[]) {
    return desc.map(paragraph => `<p>${paragraph}</p>`).join('');
  }
  
  //<div :class="trait.desc !== null ? 'tooltip' : null">
  //    {{trait.name}}
  //    <span class="tooltiptext" v-if="trait.desc !== null">
                            //        {{trait.desc}}
  //    </span>
  //</div>
  
  notNullSingleton(val: any) {
    return (val !== null) ? [val] : [];
  }
}

