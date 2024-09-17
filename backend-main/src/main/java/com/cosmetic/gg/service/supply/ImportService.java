package com.cosmetic.gg.service.supply;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.supply.ImportDetailResponse;
import com.cosmetic.gg.entity.supply.Import;
import com.cosmetic.gg.model.supply.ImportModel;

public interface ImportService {

	Map<String, Object> search(String keyword, String supplierId, 
			LocalDateTime importDateFrom, LocalDateTime importDateTo,
			Integer pageIndex, Integer pageSize);
	
	List<Error> validator(ImportModel importModel);
	
	Import create(ImportModel importModel);
	
	Import update(ImportModel importModel);
	
	Error delete(String id);
	
	ImportDetailResponse detail(String id);
}
