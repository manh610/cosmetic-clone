package com.cosmetic.gg.repository.supply;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.supply.Import;

@Repository
public interface ImportRepository extends JpaRepository<Import, String>{

	@Query(value = "SELECT * FROM import t WHERE (t.id=:key OR t.code=:key) "
		    , nativeQuery = true)
	Import findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM import t WHERE " +
			"(CASE WHEN :supplierId IS NOT NULL THEN t.supplier_id=:supplierId ELSE (1=1) END) AND " +
			"(CASE WHEN (:importDateFrom IS NOT NULL AND :importDateTo IS NOT NULL) THEN " +
			"(t.import_date>=:importDateFrom AND t.import_date<=:importDateTo) ELSE (t.id IS NOT NULL) END) AND " +
			"(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(t.code REGEXP :keyword " +
		    "OR t.representative_name REGEXP :keyword " +
		    "OR t.representative_phone REGEXP :keyword " +
		    "OR t.representative_email REGEXP :keyword) " +
		    "ELSE (t.id IS NOT NULL) END) " +
		    "ORDER BY t.import_date DESC LIMIT :pageSize OFFSET :pageIndex"
			, nativeQuery = true)
	List<Import> search(@Param("keyword") String keyword,
						@Param("supplierId") String supplierId,
						@Param("importDateFrom") LocalDateTime importDateFrom,
						@Param("importDateTo") LocalDateTime importDateTo,
						@Param("pageIndex") Integer pageIndex,
			            @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM import t WHERE " +
			"(CASE WHEN :supplierId IS NOT NULL THEN t.supplier_id=:supplierId ELSE (1=1) END) AND " +
			"(CASE WHEN (:importDateFrom IS NOT NULL AND :importDateTo IS NOT NULL) THEN " +
			"(t.import_date>=:importDateFrom AND t.import_date<=:importDateTo) ELSE (t.id IS NOT NULL) END) AND " +
			"(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(t.code REGEXP :keyword " +
		    "OR t.representative_name REGEXP :keyword " +
		    "OR t.representative_phone REGEXP :keyword " +
		    "OR t.representative_email REGEXP :keyword) " +
		    "ELSE (t.id IS NOT NULL) END) "
			, nativeQuery = true)
	Integer cntImport(@Param("keyword") String keyword,
						@Param("supplierId") String supplierId,
						@Param("importDateFrom") LocalDateTime importDateFrom,
						@Param("importDateTo") LocalDateTime importDateTo);
}
