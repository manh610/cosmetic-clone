package com.cosmetic.gg.service.statistic;

import java.util.List;
import java.util.Map;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.dto.response.StatisticResponse;

public interface StatisticService {

	Map<String, Object> newest(String keyword, EStatus status, String brandId, String categoryId, String skinTypeId, 
			Float min, Float max, Boolean isDate, Integer pageIndex, Integer pageSize);
	
	List<StatisticResponse> topSellBrandGraph(EStatus status);
	
//	List<StatisticResponse> topSellBrand(EStatus status);
	
	List<StatisticResponse> topSellSkinType();
	
	List<StatisticResponse> topSellCategory(String categoryRootId);
}
