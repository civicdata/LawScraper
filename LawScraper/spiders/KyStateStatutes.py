# -*- coding: utf-8 -*-
import scrapy
import re

def roman_to_int(roman):
    raw = {'I':1,'V':5,'X':10,'L':50,'C':100,'D':500,'M':1000}
    res = 0
    last = 0
    for current in roman:
        con = raw[current]
        if con > last:
            res -= last
        else:
            res += last
        last = con
    return res + last


def savepath(meta):
    return re.sub(" ", "_", meta['title_name']) + "/" + re.sub(" ", "_", meta['chapter_name'])

def title_index(title_name):
    return roman_to_int(re.search("(?P<ind>[IVXLCDM]+) - ",title_name).group('ind'))


class KystatestatutesSpider(scrapy.Spider):
    name = "KyStateStatutes"
    allowed_domains = ["www.lrc.ky.gov"]
    start_urls = (
        'http://www.lrc.ky.gov/statutes/',
    )


    def parse(self, response):
        for title in response.xpath("//div[@id='Panel1']/span/ul/li/span"):
            title_name = title.xpath("text()").extract()[0]
            title_i = title_index(title_name)
            # The next line also selects subtitles
            for chapter in title.xpath("parent::li/following-sibling::ul[1]//a"):
                chapter_name = chapter.xpath("text()").extract()[0]
                if len(chapter.xpath("./@href").extract()) == 1:
                    chapter_link = chapter.xpath("./@href").extract()[0]
                    pass_along = {'title_name': title_name, 'title_index': title_i,
                                  'chapter_name':chapter_name}
                    yield scrapy.Request(response.urljoin(chapter_link),self.parse_chapter,meta={'passed': pass_along})

    def parse_chapter(self,response):
        for subchapter in response.xpath("//ul/li//a"):
            subchapter_name = subchapter.xpath("text()").extract()[0]
            subchapter_ind = re.search("^[.](?P<ind>[^\s]+)",subchapter_name).group('ind')
            subchapter_link = response.urljoin(subchapter.xpath("./@href").extract()[0])
            item = response.meta['passed'].copy()
            item['subchapter_name'] = subchapter_name
            item['subchapter_index'] = subchapter_ind
            item['subchapter_link'] = subchapter_link
            item['file_urls'] = [subchapter_link]
            yield item
