import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SheetComponent } from './sheet.component.ts~';

describe('SheetComponent', () => {
  let component: SheetComponent;
  let fixture: ComponentFixture<SheetComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SheetComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SheetComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
