import { Component, Input, OnInit } from '@angular/core';
import { Observable, map, take } from 'rxjs';
import { CharacterService } from '../../character.service';

@Component({
  selector: 'app-spell-prepared',
  templateUrl: './spell-prepared.component.html',
  styleUrls: ['./spell-prepared.component.css']
})
export class SpellPreparedComponent implements OnInit {
  @Input() origin: string;
  @Input() spellName: string;
  @Input() prepared$: Observable<boolean | 'always'>;

  prepared: boolean;

  constructor(private charService: CharacterService) {}

  ngOnInit() {
    this.prepared$.subscribe((prep) => {
      console.log(prep);
      // TODO is this check needed?
      if (prep !== this.prepared && prep !== 'always') this.prepared = prep;
    });
  }

  update(ev: boolean): void {
    this.charService.setSpellPreparedness(this.origin, this.spellName, ev);
  }
}
