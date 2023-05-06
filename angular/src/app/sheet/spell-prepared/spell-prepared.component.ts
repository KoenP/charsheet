import { Component } from '@angular/core';

@Component({
  selector: 'app-spell-prepared',
  templateUrl: './spell-prepared.component.html',
  styleUrls: ['./spell-prepared.component.css']
})
export class SpellPreparedComponent {
  @Input() maybeToggle: BehaviorSubject<boolean> | 'always';

