import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SpellPreparedComponent } from './spell-prepared.component.ts~';

describe('SpellPreparedComponent', () => {
  let component: SpellPreparedComponent;
  let fixture: ComponentFixture<SpellPreparedComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SpellPreparedComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SpellPreparedComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
