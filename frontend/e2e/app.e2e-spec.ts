import { PlazaPage } from './app.po';

describe('plaza App', () => {
  let page: PlazaPage;

  beforeEach(() => {
    page = new PlazaPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
